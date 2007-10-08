augment
    by Phil Hagelberg (c) 2007
    http://augment.rubyforge.org

== Description
  
Augment is a framework for gathering metadata from code and displaying
it. This metadata would include test failures, test coverage levels,
complexity metrics, and others. Display frontends will be pluggable so
as to interface with many editors.

== Usage

== Design

Augment is designed to be generalized; if you want to collect
different kinds of data you can just write a new backend. If you want
to display the data in an unsupported editor, you can write a new
frontend.

=== Backends

Each backend takes a filename or glob and outputs the metadata about
that file into the .augment directory, created in the project root.
Metadata is stored as layer information in JSON. Each backend also
needs an initiator command to kick off the process that gathers the
metadata, with the exception of auto-initiating backends like autotest.

=== Frontends

Each frontend is implemented within an editor or IDE. It watches the
.augment directory for changes and applies them to the open buffers
when they happen.  The user can choose what augmentations he wants
displayed either globally or on a per-file basis. He can also control
when backend initiators should be started.

=== Layers

A layer consists of a single piece of metadata about a file. This is
expressed in terms of the range it applies to in the file, its color,
and the message associated with it.

== Support

Backends:

* Tests or specs
* Heckle (planned)
* Rcov (planned)
* Flog (planned)
* Performance (planned)

Frontends:

* Emacs
* ANSI color codes (for shells)
* Vim (planned)
* Textmate (planned)

== Issues
  
* Not really written yet.

== License

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING. If not, write to the
Free Software Foundation at this address:

  Free Software Foundation
  51 Franklin Street, Fifth Floor
  Boston, MA 02110-1301
  USA
