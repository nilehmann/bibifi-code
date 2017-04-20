Overview
========

This is the codebase for Build-it Break-it Fix-it (BIBIFI). 
We have run the contest on RHEL and Ubuntu systems. 
VMs can be provisioned via Docker Swarm or AWS EC2. 

Infrastructure
--------------

__bibifi-webapp__ - Code for the website. 

__bibifi-translator__ - CLI program to interact with the contest's database, insert submissions, etc.

__bibifi-runner__ - Program that waits for submissions, starts VM images, tests submissions, grades participants, etc.

__bibifi-periodic__ - Script that periodically pulls the git repository for each participant. If there are new submissions, `periodic` calls `translator` to insert the submission into the database.

__bibifi-core__ - Library code shared by the webapp, runner, and translator.


Setup
=====

PostgreSQL
----------

	sudo postgresql-setup initdb
	sudo chkconfig postgresql on
	sudo service postgresql start

	sudo -u postgres -i
	psql


BIBIFI uses PostgreSQL as its database backend. 
Here's some SQL to setup your PostgreSQL database. Be sure to set `YOURPASSWORD`!

	CREATE USER bibifi WITH PASSWORD 'YOURPASSWORD';
	CREATE DATABASE bibifi OWNER bibifi ENCODING 'UTF8';

To enable password logins edit the following:

	sudo vim /var/lib/pgsql/data/pg_hba.conf

Change
	local  all      all          peer
To
	local  all      all          md5 
# TODO: other foreign connections

Finish configuring the database:

	sudo service postgresql restart
	psql -U bibifi

	\c bibifi bibifi
	# CONNECT TO bibifi AS bibifi;
	REVOKE ALL ON DATABASE bibifi FROM public;
	SET timezone='UTC';

You'll need to update the `config/postgresql.yml` configuration file with your database's `username`, `password`, `host`, and `port` information.

Core
----

`bibifi-core` is library code shared by the webapp, runner, and translator.
To compile the library, you will need to install [stack](https://haskellstack.org). 
Then you can run `stack build` in the `bibifi-core` directory:

	cd bibifi-core/
	stack build

Webapp
------

TODO `config/settings.yml`

Once `bibifi-core` is built, you can compile the web application by running `stack build` in the `bibifi-webapp` directory:

	cd bibifi-webapp/
	stack build

Run executable:

	stack exec -- bibifi-webapp Production --port YOURPORT

To create a contest, visit `/admin/contests/create` and fill out the form. 

Runner
------

	cd bibifi-runner/
	sudo yum install libssh2-devel
	stack build

Contest Problems
================

Contest problems are composed of:

- A problem specification that describes the programs contestants are expected to implement. The specification should include instructions for how to make submissions and how submissions will be graded. 
- A suite of tests that include correctness, performance, and optional tests. 
- A virtual machine image that is used to test submissions. 
- A grading script that can run on the virtual machine to run tests, oracle submissions, and break-it tests.

We have used three different problem specifications in the past.
One problem is a secure log that tracks and queries the movement of entities through an art gallery.
Another problem is an ATM server that accepts withdraw, deposit, and account creation requests from a client application.
The final problem is a queryable database that supports permissions and access control checks.
If you are interested in using one of these existing projects, [email us](mailto:info@builditbreakit.org) and we can probably share the problem materials that we have.

If you would like to integrate your own problem specifications into the infrastructure, you can use the [Contest Problem API](docs/API.md). 

Running Contests
================

TODO...

You can zip up build submissions by running the following in the `repos` directory:

    for D in *; do zip -r $D.zip $D -x *.git*; done;
		mv *.zip ../round2/

