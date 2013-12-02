
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(rj_schema).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("rj_schema.hrl").
-import(xmerl_xs, [select/2]).
-export([from_xml/1, to_xml/1, from_document/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

from_xml(XMLSchema) ->
    {XMLContent,_} = xmerl_scan:string(XMLSchema),
    DecodedJson = extract_user_fields(XMLContent),
    mochijson2:encode(DecodedJson).

to_xml(JSchema) ->
    UserSchema = mochijson2:decode(JSchema),
    {NewFields, NewCopyFields} = generate_user_fields(UserSchema, {[],[]}),

    lists:flatten(
        ?DEFAULT_SCHEMA_HEADER ++
        xmerl:export_simple_content(NewFields,xmerl_xml) ++
        ?DEFAULT_SCHEMA_MIDDLE ++
        xmerl:export_simple_content(NewCopyFields,xmerl_xml) ++
        ?DEFAULT_SCHEMA_FOOTER
    ).

from_document(JDocument) ->
    {_, DocProps} = mochijson2:decode(binary_to_list(JDocument)),
    mochijson2:encode(lists:map(
      fun({K, V}) ->
          PropsList = [{<<"name">>, K}],
          Type =
          if
              is_integer(V) orelse is_float(V) -> <<"number">>;
              is_binary(V) -> string_or_text_type_for(V)
          end,
          {struct, [{<<"type">>, Type} | PropsList]}
      end,
      DocProps)).

%%% =================================================== internal functions

generate_user_fields([], NewData) ->
    NewData;
generate_user_fields([{_,Props}|Rest],{Fields, CopyFields}) ->
    Name = binary_to_list(proplists:get_value(<<"name">>, Props)),
    Type = binary_to_list(proplists:get_value(<<"type">>, Props)),

    {NewField, NewCopyField} = case Type of
        "text" ->
            {[{field, [{name, Name}, {type, solr_type(Type)}, {indexed, "true"}, {stored, "false"}],[]}],
             [{copyField, [{source, Name}, {dest, "text"}],[]}]};
        "string" ->
            {[{field, [{name, Name}, {type, solr_type(Type)}, {indexed, "true"}, {stored, "false"}],[]}],
             [{copyField, [{source, Name}, {dest, "text"}],[]}]};
        "multi_string" ->
            {[{field, [{name, Name}, {type, solr_type(Type)}, {indexed, "true"}, {stored, "false"}, {multiValued, "true"}],[]}],
             [{copyField, [{source, Name}, {dest, "text"}],[]}]};
        "number" ->
            {[{field, [{name, Name}, {type, solr_type(Type)}, {indexed, "true"}, {stored, "false"}],[]}],[]};
        "integer" ->
            {[{field, [{name, Name}, {type, solr_type(Type)}, {indexed, "true"}, {stored, "false"}],[]}],[]};
        "geo" ->
            {[{field, [{name, Name}, {type, solr_type(Type)}, {indexed, "true"}, {stored, "false"}, {multiValued, "true"}],[]}],[]}
    end,

    generate_user_fields(Rest, {Fields ++ NewField, CopyFields ++ NewCopyField}).

extract_user_fields(E = #xmlElement{name='schema'}) ->
    extract_user_fields(select("fields/field",E), []).

extract_user_fields([E = #xmlElement{name='field'}|Rest], UserFields) ->
    extract_user_fields(Rest, UserFields ++ maybe_user_field(E));
extract_user_fields([], UserFields) ->
    UserFields.

get_attribute_value(Element, Name) when is_record(Element, xmlElement) ->
    get_attribute_value(Element#xmlElement.attributes, Name);
get_attribute_value([A = #xmlAttribute{name=Name}|_], Name) ->
    A#xmlAttribute.value;
get_attribute_value([], _) ->
    {error, notfound};
get_attribute_value([_|Rest], Name) ->
    get_attribute_value(Rest, Name).

solr_type(Type) ->
    case Type of
        "text" -> "text_general";
        "string" -> "string";
        "multi_string" -> "string";
        "number" -> "tdouble";
        "integer" -> "int";
        "geo" -> "location"
    end.

user_type("string", "true") -> "multi_string";
user_type("string", _) -> "string";
user_type(Type, _) ->
    case Type of
        "text" -> "text_general";
        "text_general" -> "text";
        "tdouble" -> "number";
        "int" -> "integer";
        "location" -> "geo"
    end.

maybe_user_field(E) ->
    case is_user_field(E) of
        false -> [];
        true ->
            [{struct,[{<<"name">>,list_to_binary(get_attribute_value(E, 'name'))},
             {<<"type">>, list_to_binary(user_type(
                get_attribute_value(E, 'type'),
                get_attribute_value(E, 'multiValued')))}]}]
    end.

is_user_field(Element) ->
    case get_attribute_value(Element, 'name') of
        {error, notfound} -> false;
        Name -> not lists:member(Name, ?NON_USER_FIELDS)
    end.

string_or_text_type_for(V) ->
    case re:run(V, "\s") of
    {match, _} -> <<"text">>;
    _ -> <<"string">>
    end.






-ifdef(TEST).

integer_test() ->
    Input = <<"{\"foo\": 42}">>,
    Expected = mochijson2:decode("[{\"type\":\"number\",\"name\":\"foo\"}]"),
    ?assertEqual(Expected, mochijson2:decode(rj_schema:from_document(Input))).

string_test() ->
    Input = <<"{\"foo\": \"bar\"}">>,
    Expected = mochijson2:decode("[{\"type\":\"string\",\"name\":\"foo\"}]"),
    ?assertEqual(Expected, mochijson2:decode(rj_schema:from_document(Input))).

text_test() ->
    Input = <<"{\"foo\": \"bar blech\"}">>,
    Expected = mochijson2:decode("[{\"type\":\"text\",\"name\":\"foo\"}]"),
    ?assertEqual(Expected, mochijson2:decode(rj_schema:from_document(Input))).

json_to_xml_test() ->
    Input = ?USER_SCHEMA_JSON,
    Expected = ?USER_TEST_SCHEMA_XML,
    ?assertEqual(Expected, rj_schema:to_xml(Input)).

xml_to_json_test() ->
    Input = ?USER_TEST_SCHEMA_XML,
    Expected = ?USER_SCHEMA_JSON,
    ?assertEqual(mochijson2:decode(Expected),
        mochijson2:decode(rj_schema:from_xml(Input))).

full_translate_test() ->
    Input = ?USER_SCHEMA_JSON,
    XML = rj_schema:to_xml(Input),
    Output = rj_schema:from_xml(XML),
    ?assertEqual(mochijson2:decode(Input), mochijson2:decode(Output)).

empty_schema_to_xml_test() ->
    Input = "[]",
    Expected = ?DEFAULT_SCHEMA_XML,
    ?assertEqual(Expected, rj_schema:to_xml(Input)).

empty_schema_from_xml_test() ->
    Input = ?DEFAULT_SCHEMA_XML,
    Expected = <<"[]">>,
    ?assertEqual(Expected, rj_schema:from_xml(Input)).

-endif.
