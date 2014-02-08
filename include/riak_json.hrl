
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.
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


-define(any_to_binary(Str), case is_binary(Str) of true -> Str; _ -> list_to_binary(Str) end).
-define(any_to_list(Str), case is_binary(Str) of true -> binary_to_list(Str); _ -> Str end).

-define(RJ_INDEX_POSTFIX, "RJIndex").

-define(RJ_TYPE_POSTFIX, "RJType").

-define(RJ_SCHEMA_POSTFIX, "DefaultSchema").

-define(RJ_INDEX(COL),?any_to_list(COL) ++ ?RJ_INDEX_POSTFIX).

-define(RJ_TYPE(COL), ?any_to_list(COL) ++ ?RJ_TYPE_POSTFIX).

-define(RJ_SCHEMA(COL), ?any_to_list(COL) ++ ?RJ_SCHEMA_POSTFIX).