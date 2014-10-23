=head1 NAME

OpenERP::OOM::Roles::Class - Class attribute for setting up dirty attribute tracking.

=head1 DESCRIPTION

This code was largely taken from a version of MooseX::TrackDirty before it 
was updated to work with Moose 2.0.  Then it was cut down to suit our purposes
being uses in the Moose::Exporter.


=head1 LICENSE AND COPYRIGHT

Copyright (C) 2011 OpusVL

This library is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=cut

package OpenERP::OOM::Roles::Class;
use namespace::autoclean;
use Moose::Role;

has __track_dirty => (
    traits => [ 'Hash' ],
    is      => 'rw',
    isa     => 'HashRef',
    builder => '__build_track_dirty',

    handles => {
        is_dirty             => 'exists',
        mark_clean           => 'delete',
        mark_all_clean       => 'clear',
        has_dirty_attributes => 'count',
        all_attributes_clean => 'is_empty',
        dirty_attributes     => 'keys',
        _set_dirty           => 'set',
   },
);   

sub __build_track_dirty { { } }
sub _mark_dirty { shift->_set_dirty(shift, 1) }

1;

