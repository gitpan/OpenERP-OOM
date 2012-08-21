use inc::Module::Install;

name     'OpenERP-OOM';
all_from 'lib/OpenERP/OOM.pm';
author   q{Jon Allen (JJ) <jj@opusvl.com>};
license 'perl';

build_requires 'Test::More';

requires 'Moose';
requires 'Moose::Exporter';
requires 'Moose::Role';
requires 'Moose::Util::MetaRole';
requires 'Moose::Util::TypeConstraints';
requires 'MooseX::Role::XMLRPC::Client';
requires 'List::MoreUtils';
requires 'RPC::XML';
requires 'DateTime';
requires 'DateTime::Format::Strptime';

requires 'OpenERP::XMLRPC::Simple';
requires 'MooseX::NotRequired';

auto_install;

WriteAll;

