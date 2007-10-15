augment
    by Phil Hagelberg (c) 2007
    http://augment.rubyforge.org

== Description
  
Augment is a framework for gathering metadata from code and displaying
it. This metadata would include test failures, test coverage levels,
complexity metrics, and others. Display frontends will be pluggable so
as to interface with many editors.

== Usage

The +augment+ executable gathers metadata in the form of layers for a
given file via a backend. Some backends gather data for a file other
than the original one passed in. (The test backend will store data for
the test if you pass in the implementation.)

Example:

$ augment test lib/foo.rb # will store metadata for test/test_foo.rb

The various frontends have their own executables that provide access
to the layer metadata once it's stored. The simplest frontend is
+augment_color+ which outputs the layers via ANSI color codes to the
shell:

$ augment_color test/test_foo.rb

Most other frontends are editor-specific.

== Design

Augment is designed to be generalized; if you want to collect
different kinds of data you can just write a new backend. If you want
to display the data in an unsupported editor or output format, you can
write a new frontend.

=== Backends

Each backend takes a filename or glob and outputs the metadata about
that file into the .augment directory, created in the project root.
Metadata is stored as layer information in JSON. Each backend also
needs an initiator command to kick off the process that gathers the
metadata, with the exception of auto-initiating backends like autotest.

=== Frontends

Most frontends are implemented within an editor or IDE. They watch
the .augment directory for changes and apply them to the open buffers
when they happen.  The user can choose what augmentations he wants
displayed either globally or on a per-file basis. He can also control
when backend initiators should be started, though usually these will
be kicked off automatically upon saving a file.

=== Layers

A layer consists of a single piece of metadata about a file. This is
expressed in terms of the range it applies to in the file, its color,
and the message associated with it.

== Support

Backends:

* Tests (miniunit)
* Rspec (planned)
* Heckle (planned)
* Rcov (planned)
* Flog (planned)
* Performance (planned)

Frontends:

* ANSI color codes (for shells)
* Emacs (planned)
* Vim (planned)
* Textmate (planned)

== Issues
  
* Not really, um, written yet.

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
