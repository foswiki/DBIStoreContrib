#---+ Extensions
#---++ DBIStoreContrib
# ** SELECTCLASS Foswiki::Contrib::DBIStoreContrib::Personality::* **
# Different implementations of SQL have different behaviours and extend
# the standard in different ways, so we have to map certain operations in
# a way that is specific to the database type. For this we use a <em>personality
# module</em>. Select the appropriate module for your database type,
# or add your own (and contribute it back).
$Foswiki::cfg{Extensions}{DBIStoreContrib}{Personality} = 'Foswiki::Contrib::DBIStoreContrib::Personality::SQLite';
# **STRING 120**
# DBI DSN to use to connect to the database. Example DSNs:
#   * dbi:ODBC:foswiki
#   * dbi:Pg:dbname=foswiki
#   * dbi:mysql:database=foswiki;host=mysqlserver;port=1234
$Foswiki::cfg{Extensions}{DBIStoreContrib}{DSN} = 'dbi:SQLite:dbname=$Foswiki::cfg{WorkingDir}/dbcache';
# **STRING 80 CHECK="undefok"**
# Optional username to use to connect to the database.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{Username} = '';
# **STRING 80 CHECK="undefok"**
# Optional password to use to connect to the database.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{Password} = '';
# **STRING 80 CHECK="undefok"**
# Optional prefix to add to SQL table names. This may be required if the
# simple table names conflict with existing tables in the database.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{TablePrefix} = '';
# **STRING 120 EXPERT CHECK="undefok"**
# Trace options - see tools/dbistore_manage.pl --help trace for a full list
$Foswiki::cfg{Extensions}{DBIStoreContrib}{Trace} = '';
# **STRING 80**
# Plugin module name (required on Foswiki 1.1 and earlier)
$Foswiki::cfg{Plugins}{DBIStorePlugin}{Module} = 'Foswiki::Plugins::DBIStorePlugin';
# **BOOLEAN**
# Plugin enable switch (required on Foswiki 1.1 and earlier)
$Foswiki::cfg{Plugins}{DBIStorePlugin}{Enabled} = 1;
# **STRING 80 CHECK="undefok"**
# Where to find the PCRE library for SQLite. Only used by SQLite. It is
# installed on Debian Linux using apt-get install sqlite3-pcre
# (or similar on other distributions).
$Foswiki::cfg{Extensions}{DBIStoreContrib}{SQLite}{PCRE} = '/usr/lib/sqlite3/pcre.so';
#  **BOOLEAN CHECK="undefok"**
# Set to true to automatically create new tables when unregistered META is
# encountered in topic text. This should not normally be required, as plugins
# should register all META that they create. Note that only META:NAME where
# NAME matches /^[A-Z][A_Z0-9_]+$/ will be loaded.
# Note that searching fields that are created 'on the fly' is potentially
# risky, as if the field is missing from a topic it will not be present
# in the table, so finding topics without that field becomes tricky.
# It is always better to register META.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{AutoloadUnknownMETA} = 0;
#  **BOOLEAN CHECK="undefok"**
# Set to true to automatically create new columns when fields in META are
# not present in the schema. This should not normally be required, as plugins
# should register all META that they create.
# Note that searching fields that are created 'on the fly' is potentially
# risky, as if the field is missing from a topic it will not be present
# in the table, so finding topics without that field becomes tricky.
# It is always better to register META.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{AutoAddUnknownFields} = 0;
# **PERL EXPERT CHECK="undefok"**
# Set values for extra $dbh->{attributes} to be set when the connection
# has been made. You can use this (for example) to set odbc_ attributes when
# using the ODBC driver. See the DBI and DBD documentation for your
# specific driver for more information on the attributes that are
# available.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{DBIAttributes} = {};
# **PERL**
# Specify how to construct the database. Some databases may require a
# different schema, or plugins may require an extension to the schema.
# See http://foswiki.org/Extensions/DBIStoreContrib for a detailed discussion.
# Note especially the function of the FILEATTACHMENT.text field.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema} = {
  _DEFAULT => { type => 'TEXT', default => '' },
  _INDEXED => { type => 'TEXT', index => 1 },
  FIELD => {
    tid  => { type => 'INT' },
    name => '_INDEXED',
    value => '_DEFAULT',
    title => '_DEFAULT'
  },
  FILEATTACHMENT => {
    tid  => { type => 'INT' },
    name => '_INDEXED',
    version => '_DEFAULT',
    path => '_DEFAULT',
    size => '_DEFAULT',
    date => '_DEFAULT',
    user => '_INDEXED',
    comment => '_DEFAULT',
    attr => '_DEFAULT',
    text => '_DEFAULT'
  },
  FORM => {
    tid  => { type => 'INT' },
    name => '_INDEXED'
  },
  PREFERENCE => {
    tid  => { type => 'INT', unique => 'onepref' },
    name => { basetype => '_INDEXED', unique => 'onepref' },
    value => '_DEFAULT',
    type => '_DEFAULT',
  },
  TOPICINFO => {
    tid  => { type => 'INT', unique => 'onetopicinfo' },
    author => '_INDEXED',
    version => '_DEFAULT',
    date => '_DEFAULT',
    format => '_DEFAULT',
    reprev => '_DEFAULT',
    rev => '_DEFAULT',
    comment => '_DEFAULT'
  },
  TOPICMOVED => {
    tid  => { type => 'INT' },
    from => '_DEFAULT',
    to => '_DEFAULT',
    by => '_DEFAULT',
    date => '_DEFAULT',
  },
  TOPICPARENT => {
    tid  => { type => 'INT', unique => 'oneparent' },
    name => '_INDEXED'
  },
  topic => {
    tid  => { type => 'INT', primary => 1 },
    web  => { basetype => '_INDEXED', unique => 'webtopic' },
    name => { basetype => '_INDEXED', unique => 'webtopic' },
    timestamp => { type => 'INT' },
    text => '_DEFAULT',
    raw  => '_DEFAULT'
  },
  metatypes => {
    name => { type => 'TEXT', primary => 1 },
  }
};
1;
