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
# DBI DSN to use to connect to the database.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{DSN} = 'dbi:SQLite:dbname=$Foswiki::cfg{WorkingDir}/dbcache';
# **STRING 80 CHECK="undefok"**
# Optional username to use to connect to the database.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{Username} = '';
# **STRING 80 CHECK="undefok"**
# Optional password to use to connect to the database.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{Password} = '';
# **STRING 120 EXPERT**
# Trace options - see tools/dbistore_manage.pl --help trace for a full list
$Foswiki::cfg{Extensions}{DBIStoreContrib}{Trace} = '';
# **PERL**
# Default attributes to pass to the DBI->connect call that connects to your
# database. You may need to customise these for your specific install,
# depending on the data types used in your schema. Search Google for
# "DBI Connect Attributes" for more details.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{Connect} = { RaiseError => 1, AutoCommit => 1 };
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
#  **BOOLEAN**
# Set to true to automatically create new tables when unregistered META is
# encountered in topic text. This should not normally be required, as plugins
# should register all META that they create. Note that only META:NAME where
# NAME matches /^[A-Z][A_Z0-9_]+$/ will be loaded.
# Note that searching fields that are created 'on the fly' is potentially
# risky, as if the field is missing from a topic it will not be present
# in the table, so finding topics without that field becomes tricky.
# It is always better to register META.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{AutoloadUnknownMETA} = 0;
#  **BOOLEAN**
# Set to true to automatically create new columns when fields in META are
# not present in the schema. This should not normally be required, as plugins
# should register all META that they create.
# Note that searching fields that are created 'on the fly' is potentially
# risky, as if the field is missing from a topic it will not be present
# in the table, so finding topics without that field becomes tricky.
# It is always better to register META.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{AutoAddUnknownFields} = 0;
# **PERL**
# Specify how to construct the database. This schema should work with
# SQlite3 and PostgreSQL but other database may require a different schema,
# or plugins may require an extension to the schema.
# See http://foswiki.org/Extensions/DBIStoreContrib for a detailed discussion.
$Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema} = {
    _DEFAULT => { type => 'TEXT' },
    _INDEXED => { type => 'TEXT', index => 1 },
    topic => {
        tid  => { type => 'INT', primary => 1 },
        web  => { type => 'TEXT', index => 1, unique => 'webtopic' },
        name => { type => 'TEXT', index => 1, unique => 'webtopic' },
        text => '_DEFAULT',
        raw  => '_DEFAULT'
        },
    metatypes => {
        name => { type => 'TEXT', primary => 1 },
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
    FILEATTACHMENT => {
        tid  => { type => 'INT' },
        name => '_INDEXED',
        version => '_DEFAULT',
        path => '_DEFAULT',
        size => '_DEFAULT',
        date => '_DEFAULT',
        user => '_INDEXED',
        comment => '_DEFAULT',
        attr => '_DEFAULT'
    },
    FORM => {
        tid  => { type => 'INT' },
        name => { type => 'TEXT', index => 1 },
    },
    FIELD => {
        tid  => { type => 'INT' },
        name => '_INDEXED',
        value => '_DEFAULT',
        title => '_DEFAULT'
    },
    PREFERENCE => {
        tid  => { type => 'INT', unique => 'onepref' },
        name => { type => 'TEXT', index => 1, unique => 'onepref' },
        value => '_DEFAULT',
        type => '_DEFAULT',
    }
};
1;
