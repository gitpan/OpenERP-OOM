package OpenERP::OOM::Schema;
=head1 NAME

OpenERP::OOM::Schema

=head1 SYNOPSYS

=head1 DESCRIPTION

=head1 METHODS

=head2 class

Provides the OpenERP::OOM::Class class object requested.  This will
load the class into memory.

    my $accounts = $schema->class('Accounts');

=head1 PROPERTIES

=head2 openerp_connect

This should be populated with a hash of the OpenERP connection details.

        <openerp_connect>
            username admin
            password admin
            dbname company-database
            host openerp-server
        </openerp_connect>

=head2 link_provider

Out of the box the links to the external database are generated each time the 
link is followed.  It's generally a good idea to provide your own provider that
provides a cached connection for the link.

NOTE: this could do with more detail to explain it better.

=head2 link_config

This is the configuration for the externals links.  Typcially it is setup with
the connection information if the default link provider is used.  If another link
provider is provided this won't be necessary.

=head2 client

The XMLRPC client that talks to OpenERP.

=head2 link

Provides a link to another part of the database.

    $schema->link('DBIC')

=head2 provide_link

A default implementation of a link provider that loads up an OpenERP::OOM::Link::$class
on the fly.  This is slow so you normally don't want to use this.

=head1 COPYRIGHT & LICENSE

Copyright (C) 2011 OpusVL

This library is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=cut

use 5.010;
use Moose;
use OpenERP::XMLRPC::Client;

with 'OpenERP::OOM::DynamicUtils';
with 'OpenERP::OOM::Link::Provider';

has 'openerp_connect' => (
    isa => 'HashRef',
    is  => 'ro',
);

has 'link_config' => (
    isa => 'HashRef',
    is  => 'ro',
);

has link_provider => (
    isa => 'OpenERP::OOM::Link::Provider',
    is => 'ro',
    lazy_build => 1,
);

has 'client' => (
    isa     => 'OpenERP::XMLRPC::Client',
    is      => 'ro',
    lazy    => 1,
    builder => '_build_client',
);

sub _build_link_provider
{
    # we are also a link provider
    # so use that if one isn't provided.
    my $self = shift;
    return $self;
}

#-------------------------------------------------------------------------------

sub _build_client {
    my $self = shift;
    
    return OpenERP::XMLRPC::Client->new(%{$self->openerp_connect});
}


#-------------------------------------------------------------------------------

sub class {
    my ($self, $class) = @_;
    
    my $package = $self->meta->name . "::Class::$class";
    my $object_package = $self->meta->name . "::Object::$class";
    
    $self->ensure_class_loaded($package);
    $self->ensure_class_loaded($object_package);
    
    return $package->new(
        schema => $self,
    );
}


#-------------------------------------------------------------------------------

sub link 
{
    my ($self, $class) = @_;

    return $self->link_provider->provide_link($class);
}

sub provide_link {
    my ($self, $class) = @_;
    
    my $package = ($class =~ /^\+/) ? $class : "OpenERP::OOM::Link::$class";

    $self->ensure_class_loaded($package);
    
    return $package->new(
        schema => $self,
        config => $self->link_config->{$class},
    );
}

1;
