package OpenERP::OOM::Class::Base;

use 5.010;
use Carp;
use Data::Dumper;
use Moose;
use RPC::XML;
use DateTime;
use DateTime::Format::Strptime;
use MooseX::NotRequired;

extends 'Moose::Object';
with 'OpenERP::OOM::DynamicUtils';

=head1 NAME

OpenERP::OOM::Class::Base

=head1 SYNOPSYS

 my $obj = $schema->class('Name')->create(\%args);
 
 foreach my $obj ($schema->class('Name')->search(@query)) {
    ...
 }

=head1 DESCRIPTION

Provides a base set of methods for OpenERP::OOM classes (search, create, etc).

=cut

has 'schema' => (
    is => 'ro',
);

has 'object_class' => (
    is      => 'ro',
    lazy    => 1,
    builder => '_build_object_class',
);

sub _build_object_class {
    my $self = shift;
    
    # if you get this blow up it probably means the class doesn't compile for some
    # reason.  Run the t/00-load.t tests.  If they pass check you have a use_ok 
    # statement for all your modules.
    die 'Your code doesn\'t compile llamma' if !$self->can('object');
    $self->ensure_class_loaded($self->object);
    
    $self->object->meta->add_method('class' => sub{return $self});
    
    return $self->object->new;
}

#-------------------------------------------------------------------------------

=head2 search

Searches OpenERP and returns a list of objects matching a given query.

    my @list = $schema->class('Name')->search(
        ['name', 'ilike', 'OpusVL'],
        ['active', '=', 1],
    );

The query is formatted as a list of array references, each specifying a
column name, operator, and value. The objects returned will be those where
all of these sub-queries match.

Searches can be performed against OpenERP fields, linked objects (e.g. DBIx::Class
relationships), or a combination of both.

    my @list = $schema->class('Name')->search(
        ['active', '=', 1],
        ['details', {status => 'value'}, {}],
    )

In this example, 'details' is a linked DBIx::Class object with a column called
'status'.

An optional 'search context' can also be provided at the end of the query list, e.g.

    my @list = $schema->class('Location')->search(
        ['usage' => '=' => 'internal'],
        ['active' => '=' => 1],
        {
            active_id => $self->id,
            active_ids => [$self->id],
            active_model => 'product.product',
            full => 1,
            product_id => $self->id,
            search_default_in_location => 1,
            section_id => undef,
            tz => undef,
        }
    );

Supplying a context further restricts the search, for example to narrow down a
'stock by location' query to 'stock of a specific product by location'.

Following the search context, an arrayref of options can be given to return a
paged set of results:

    {
        limit  => 10,    # Return max 10 results
        offset => 20,    # Start at result 20
    }

=head2 raw_search

This is the same as search but it doesn't turn the results into objects.  This 
is useful if your search is likely to have returned fields that aren't part of
the object.  Queries like those used by the Stock By Location report are likely
to return stock levels as well as the location details for example.

=cut

sub raw_search {
    my ($self, @args) = @_;
    use Data::Dumper;
    ### Initial search args: @args
    
    my @search;
    while (@args && ref $args[0] ne 'HASH') {push @search, shift @args}
    
    # Loop through each search criteria, and if it is a linked object 
    # search, replace it with a translated OpenERP search parameter.
    foreach my $criteria (@search) {
        if(ref $criteria eq 'ARRAY') {
            my $search_field = $criteria->[0];

            if (my $link = $self->object_class->meta->link->{$search_field}) {
                if ($self->schema->link($link->{class})->can('search')) {
                    my @results = $self->schema->link($link->{class})->search($link->{args}, @$criteria[1 .. @$criteria-1]);

                    if (@results) {
                        ### Adding to OpenERP search: 
                        ### $link->{key} 
                        ### IN 
                        ### join(', ', @results)
                        $criteria = [$link->{key}, 'in', \@results];
                    } else {
                        return;  # No results found, so no point searching in OpenERP
                    }
                } else {
                    carp "Cannot search for link type " . $link->{class};
                }
            }
        }
    }
    
    my $context = shift @args;
    my $options = shift @args;
    $options = {} unless $options;
    ### Search: @search
    ### Search context: $context
    ### Search options: $options
    my $objects = $self->schema->client->search_detail($self->object_class->model,[@search], $context, $options->{offset}, $options->{limit});

    if ($objects) {    
        foreach my $attribute ($self->object_class->meta->get_all_attributes) {
            if($attribute->type_constraint =~ /DateTime/)
            {
                my $parser = DateTime::Format::Strptime->new(pattern     => '%Y-%m-%d');
                map { $_->{$attribute->name} = $parser->parse_datetime($_->{$attribute->name}) } @$objects;
            }
        }
        return $objects;
    } else {
        return undef;
    }
}

sub search
{
    my $self = shift;
    my $objects = $self->raw_search(@_);
    if($objects) {
        return map {$self->object_class->new($_)} @$objects;
    } else {
        return wantarray ? () : undef;
    }
}

=head2 is_not_null

Returns search criteria for a not null search.  i.e. equivalend to $field is not null in SQL.

    $self->search($self->is_not_null('x_department'), [ 'other_field', '=', 3 ]);

=cut

sub is_not_null
{
    my $self = shift;
    my $field = shift;
    return [ $field, '!=', RPC::XML::boolean->new(0) ];
}

=head2 null

Returns a 'null' for use in OpenERP calls and objects.  (Actually this is a False value).

=cut

sub null { RPC::XML::boolean->new(0) }

=head2 is_null

Returns search criteria for an is null search.  i.e. equivalend to $field is null in SQL.

    $self->search($self->is_null('x_department'), [ 'other_field', '=', 3 ]);

=cut

sub is_null
{
    my $self = shift;
    my $field = shift;
    return [ $field, '=', RPC::XML::boolean->new(0) ];
}

#-------------------------------------------------------------------------------

=head2 find

Returns the first object matching a given query.

 my $obj = $schema->class('Name')->find(['id', '=', 32]);

Will return C<undef> if no objects matching the query are found.

=cut

sub find {
    my $self = shift;
    
    #my $ids = $self->schema->client->search($self->object_class->model,[@_]);
    my $ids = $self->raw_search(@_);
    
    if ($ids->[0]) {
        #return $self->retrieve($ids->[0]);
        return $self->object_class->new($ids->[0]);
    }
}


=head2 get_options

This returns the options for available for a selection field.  It will croak if you
try to give it a field that isn't an option.

=cut

sub get_options 
{
    my $self = shift;
    my $field = shift;

    my $model_info = $self->schema->client->model_fields($self->object_class->model);
    my $field_info = $model_info->{$field};
    croak 'Can only get options for selection objects' unless $field_info->{type} eq 'selection';
    my $options = $field_info->{selection};
    return $options;
}

#-------------------------------------------------------------------------------

=head2 retrieve

Returns an object by ID.

 my $obj = $schema->class('Name')->retrieve(32);

=cut

sub retrieve {
    my ($self, $id, @args) = @_;
    
    # FIXME - This should probably be in a try/catch block
    if (my $object = $self->schema->client->read_single($self->object_class->model, $id, @args)) 
    {
        return $self->_inflate_object($self->object, $object);
    }
}

sub _inflate_object
{
    my $self = shift;
    my $object_class = shift;
    my $object = shift;

    foreach my $attribute ($self->object_class->meta->get_all_attributes) {
        if($attribute->type_constraint =~ /DateTime/)
        {
            my $parser = DateTime::Format::Strptime->new(pattern     => '%Y-%m-%d');
            $object->{$attribute->name} = $parser->parse_datetime($object->{$attribute->name});
        }
    }
    return $object_class->new($object);
}

=head2 default_values

Returns an instance of the object filled in with the default values suggested by OpenERP.

=cut
sub default_values
{
    my $self = shift;
    my $context = shift;
    # do a default_get

    my @fields = map { $_->name } $self->object_class->meta->get_all_attributes;
    my $object = $self->schema->client->get_defaults($self->object_class->model, \@fields, $context);
    my $class = MooseX::NotRequired::make_optional_subclass($self->object);
    return $self->_inflate_object($class, $object);
}

#-------------------------------------------------------------------------------

=head2 retrieve_list

Takes a reference to a list of object IDs and returns a list of objects.

 my @list = $schema->class('Name')->retrieve_list([32, 15, 60]);

=cut

sub retrieve_list {
    my ($self, $ids, @args) = @_;
    
    if (my $objects = $self->schema->client->read($self->object_class->model, $ids, @args)) {
        foreach my $attribute ($self->object_class->meta->get_all_attributes) {
            if($attribute->type_constraint =~ /DateTime/)
            {
                my $parser = DateTime::Format::Strptime->new(pattern     => '%Y-%m-%d');
                map { $_->{$attribute->name} = $parser->parse_datetime($_->{$attribute->name}) } @$objects;
            }
        }
        return map {$self->object_class->new($_)} @$objects;
    }
}


#-------------------------------------------------------------------------------

sub _collapse_data_to_ids
{
    my ($self, $object_data) = @_;

    my $relationships = $self->object_class->meta->relationship;
    while (my ($name, $rel) = each %$relationships) {
        if ($rel->{type} eq 'one2many') {
            if ($object_data->{$name}) {
                $object_data->{$rel->{key}} = $self->_id($rel, $object_data->{$name});
                delete $object_data->{$name} if $name ne $rel->{key};
            }
        }
        
        if ($rel->{type} eq 'many2one') {
            if ($object_data->{$name}) {
                $object_data->{$rel->{key}} = $self->_id($rel, $object_data->{$name});
                delete $object_data->{$name} if $name ne $rel->{key};
            }            
        }
        if ($rel->{type} eq 'many2many') {
            if ($object_data->{$name}) {
                my $val = $object_data->{$name};
                my @ids;
                if(ref $val eq 'ARRAY')
                {
                    # they passed in an arrayref.
                    my $objects = $val;
                    @ids = map { $self->_id($rel, $_) } @$objects;
                }
                else
                {
                    # assume it's a single object.
                    push @ids, $self->_id($rel, $val);
                }
                $object_data->{$rel->{key}} = [[ 6, 0, \@ids ]];
                delete $object_data->{$name} if $name ne $rel->{key};
            }            
        }
    }
    # Force Str parameters to be object type RPC::XML::string
    foreach my $attribute ($self->object_class->meta->get_all_attributes) {
        if (exists $object_data->{$attribute->name}) {
            $object_data->{$attribute->name} = $self->prepare_attribute_for_send($attribute->type_constraint, $object_data->{$attribute->name});
        }
    }
    return $object_data;
}

sub _id
{
    my $self = shift;
    my $rel = shift;
    my $val = shift;
    my $ref = ref $val;
    if($ref)
    {
        # FIXME: this is close to what I want but I need to be doing it with the class
        # that corresponds to the relation we're delving into.
        if($ref eq 'HASH')
        {
            my $class = $self->schema->class($rel->{class});
            return [[ 0, 0, $class->_collapse_data_to_ids($val) ]];
        } 
        elsif($ref eq 'ARRAY') 
        {
            # this should allow us to do child objects too.
            my $class = $self->schema->class($rel->{class});
            my @expanded = map { [ 0, 0, $class->_collapse_data_to_ids($_) ] } @$val;
            return \@expanded;
        }
        else
        {
            return $val->id;
        }
    }
    return $val;
}

=head2 create

Creates a new instance of an object in OpenERP.

 my $obj = $schema->class('Name')->create({
     name   => 'OpusVL',
     active => 1,
 });

Takes a hashref of object parameters.

Returns the new object or C<undef> if it could not be created.

=cut

sub create {
    my ($self, $object_data, @args) = @_;

    ### Create called with initial object data: 
    ### $object_data;
    
    $object_data = $self->_collapse_data_to_ids($object_data);

    ### To
    ### $object_data;
    
    if (my $id = $self->schema->client->create($self->object_class->model, $object_data, @args)) 
    {
        return $self->retrieve($id);
    }
}


#-------------------------------------------------------------------------------

=head1 AUTHOR

Jon Allen (JJ) - L<jj@opusvl.com>

=head1 COPYRIGHT and LICENSE

Copyright (C) 2010 Opus Vision Limited

This library is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=cut

1;
