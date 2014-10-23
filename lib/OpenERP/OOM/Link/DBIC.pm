package OpenERP::OOM::Link::DBIC;

=head1 NAME

OpenERP::OOM::Link::DBIC

=head1 DESCRIPTION

Class used to link OpenERP data with data in DBIC.  

=head1 PROPERTIES

=head2 dbic_schema

This is the DBIC Schema object.  If you need a generic DBIC schema object
this is normally the simplest way to access it.

=head1 METHODS

These methods are not normally called directly.

=head2 create

Returns the new ID of a row it creates in a table using DBIC.

    my $id = $link->create({ class => 'RSName' }, $object_data);

It also ensures there is no _source property on the object.

=head2 retrieve

This is equivalent to doing a find on a ResultSet.

    my $object = $link->retrieve({ class => 'RSName' }, $id);

=head2 search

This is equivalent to doing a search on a ResultSet and then returning a list
of all the id fields.

    my @ids = $link->search({ class => 'RSName' }, $search, $options);

=head1 COPYRIGHT & LICENSE

Copyright (C) 2011 OpusVL

This library is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=cut

use 5.010;
use Moose;
use Try::Tiny;
extends 'OpenERP::OOM::Link';
with 'OpenERP::OOM::DynamicUtils';

has 'dbic_schema' => (
    is      => 'ro',
    lazy    => 1,
    builder => '_build_dbic_schema',
);

sub _build_dbic_schema {
    my $self = shift;
    
    $self->ensure_class_loaded($self->config->{schema_class});
    
    return $self->config->{schema_class}->connect(@{$self->config->{connect_info}});
}


#-------------------------------------------------------------------------------

sub create {
    my ($self, $args, $data) = @_;
    
    try {
        if ($self->dbic_schema->resultset($args->{class})->result_class->can('_source')) {
            # Because the _source() method is added to the result class, we need
            # to remove it in case it gets called during create(). If that happens,
            # we get bleed-through from one object to another (not good!)
            $self->dbic_schema->resultset($args->{class})->result_class->meta->make_mutable;
            $self->dbic_schema->resultset($args->{class})->result_class->meta->remove_method('_source');
        }
        my $object = $self->dbic_schema->resultset($args->{class})->create($data);
        ### Created linked object with ID $object->id
        return $object->id;
    } catch {
        die "Could not create linked object: $_";
    };
}


#-------------------------------------------------------------------------------

sub retrieve {
    my ($self, $args, $id) = @_;
    
    if (my $object = $self->dbic_schema->resultset($args->{class})->find($id)) {
        return $object;
    }
}


#-------------------------------------------------------------------------------

sub search {
    my ($self, $args, $search, $options) = @_;
    
    # FIXME - Foreign primary key column is hard-coded to "id"
    return map {$_->id} $self->dbic_schema->resultset($args->{class})->search($search, $options)->all;
}


#-------------------------------------------------------------------------------

1;
