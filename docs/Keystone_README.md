# Using RiakJson with OpenStack Keystone

## Contents

1. Terminology
2. Configuration
3. Development Environment
4. Testing

## Terminology

* tenant - A tenant is a collection entity that can contain a number of users
* user - A user represents an individual that uses the OpenStack system
* role - A role is used to define a link between a user and a tenant and indicate indicate permissions of the user within that tenant

Riak JSON does not currently support OpenStack ACLs.

Riak JSON does not currently support use of multiple authenticaiton servers via reseller prefixes.

## Configuration

An OpenStack tenant must be set up for Riak JSON and all Riak JSON users will belong to that tenant. An administrative user must be set up that is under the Riak JSON tenant and belongs to the OpenStack admin role.  This administrative user will be used to validate user tokens.  A role must be created which all users allowed to access Riak JSON must also be memebers of.

Riak JSON uses UUID authentication rather than the newer PKI authentication.

Add the following section to your app.config file:

	{auth, [
		{bypass, false},
		{module, rj_keystone_auth},
		%% Keystone Configuration
		{rj_keystone_auth, [
			{auth_url, "http://127.0.0.1:5000/v2.0"},
			{tokens_resource, "tokens"},
			{admin_username, <<"riak_json_admin">>},
			{admin_password, <<"riak_json_admin">>},
			{tenant, <<"riak_json">>},
			{permitted_roles, [<<"riak_json">>]}
		]}
	]}

### Bypass

Set bypass to false:

	{bypass, false}

### Authentication Module

Set the authentication module to specify the Keystone authentication module:

	{module, rj_keystone_auth}

### Keystone Authentication URL:

Set the authentication URL to the Keystone authorization endpoint:

	{auth_url, "http://127.0.0.1:5000/v2.0"}

### Tokens resource:

Set the tokens resource appropriately. The authorization URL will be concatenated with this value for token retrieval and validation.

	{tokens_resource, "tokens"}

### Riak JSON Tenant

Set the Keystone tenant which all users will reside:

	{tenant, <<"riak_json">>}

### Administrative User Credentials

Set the admin credentials who is under the Riak JSON tenant and who belongs to the admin group:

	{admin_username, <<"riak_json_admin">>}
	{admin_password, <<"riak_json_admin">>}

Set the permitted roles which will be allowed to access Riak JSON. This list must not be empty:

	{permitted_roles, [<<"riak_json">>]}

## Development Environment

For development, the DevStack environment, running in a Vagrant VM, is useful.  The specifics of using Vagrant are beyond the scope of this document.  A series of commands to set up an appropriate devstack environment follows. Also worth noting, all users created will be destoryed between cycles of stack.sh and unstack.sh.

	# Vagrant Ubuntu 12.04 box
	vagrant init box <boxname>
	 
	# 2GB memory, port forward 5000, 5001, 8000, 35357
	vim Vagrantfile
	 
	vagrant up
	 
	sudo apt-get update && sudo apt-get upgrade
	 
	sudo apt-get install git
	 
	git clone git://github.com/openstack-dev/devstack.git
	 
	cd devstack
	 
	git checkout stable/grizzly
	 
	#start/setup devstack
	./stack.sh

	# Retrieve and run the user create script. See https://gist.github.com/dankerrigan/4b378b7856f7cc3a1b1e#file-create_riak_json_users-sh for details

	wget https://gist.github.com/dankerrigan/4b378b7856f7cc3a1b1e/raw/13c30a2f012a38e59de1ce9f9b2e04dfb9ec4b0e/create_riak_json_users.sh
	chmod +x create_riak_json_users.sh
	./create_riak_json_users.sh

	#stop devstack
	./unstack.sh