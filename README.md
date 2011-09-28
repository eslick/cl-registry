# cl-registry

## A patient and clinical registry platform 

Copyright (c) 2008-2011 MIT Media Laboratory 
Portions Copyright (c) 2009-2011 the LAM Treatment Alliance

## Configuration Instructions

This code base supports multiple personalities, user interfaces,
module configurations and other features from configuration files
stored in sites/.  The current configurations support the live sites
[LAMsight](http://www.lamsight.org) and the
[International LAM Registry](http://www.lamregistry.org").

More TBD

## Installation Instructions

In order to use the "registry" script in this directory to load the
International LAM Registry system for developement, you need to get
the appropriate code and libraries from quicklisp:

    (ql:quickload '(:weblocks :montezuma :drakma 
                    :cl-markdown :langutils :cl-twitter
                    :cl-smtp :cl-l10n :local-time 
		    :parse-number))

Major Problems:
- Elephant works on SBCL & CCL
- cl-l10n / verify parsing & formatting works properly
- cl-l10n / fix format to provide hint to UI?

Minor problems:
- Upgrade to latest cl-twitter
  - Ensure we can use twitter oauth interface? 
  - Just don't use twitter for now

- Latest elephant builds CCL/SBCL?


SBCL Problems:
- cl-l10n build on SBCL?
- elephant SBCL PCL code


To load the LAMSIGHT2 system:

    cd ~/lamsight/registry
    ./registry [port]

If "port" is included, loads Swank (from ../systems/slime/)
and starts a server listening on "port" (4005 is the default port for
m-x slime-connect in Emacs).

If NOLOAD is set to "true" in the environment, does not load the
registry or library code, only registry-loader.lisp:

    NOLOAD=true ./registry [port]

In this case, the Slime package will be REGISTRY-LOADER, and you can
load the registry code with:

    (loadsys :registry)

Otherwise, the Slime package will be REGISTRY.

If REGPORT is an integer, will start up a debugging server on that
port. For example, the following will load the registry code, start a
server on port 8080, and start a Slime server on port 4005:

    REGPORT=8080 ./registry 4005

Alternatively, you can start the server from the REPL with:

    (registry-loader:start-registry 8080)

or:

    (start-registry :debug <debug>
                  :log-level <log-level>
                  :address "localhost"
                  :port 8080
                  :config <config>)

<debug> is NIL to log web site errors, :HTML to display error
information in the browser, or anything else to break into the
debugger. Default: NIL. You can set *ERROR-ACTION* to change it.

<log-level> is :ERROR, :WARNING, :INFO, or :DEBUG. If :ERROR, then
only errors will be logged to the log file
(<registry-dir>/logs/registry.log). If :WARNING, then both errors and
warnings will be logged. If :INFO or :DEBUG, then both errors and
warnings will be logged, plus explicit calls to LOG-MESSAGE with a
LEVEL of :INFO, if <log-level> is :INFO, or of :INFO or :DEBUG, if
<log-level> is :DEBUG. The default for <log-level> is :ERROR, if
<debug> is NIL, or :DEBUG otherwise. You can use SET-LOG-LEVEL to
change it.

<config> is a list of paths to configuration files, which are usually
stored in the "sites" directory. If it is a string, it will be split
on spaces to make a list of strings. Then each element will be merged
with "sites/.config" and the directory of "registry-loader.lisp" to
get a full (possibly relative) pathname. The default is
("ilr-production" "devel").

If REGCONFIG is set in the environment, it will be passed by
REGISTRY-LOADER:START-REGISTRY to REGISTRY:START-REGISTRY as the value
for the CONFIG parameter, after being split a list of strings using
"+" as a delimiter. Hence REGCONFIG="ilr-production+devel" is equivalent
to passing '("ilr-production" "devel") as the CONFIG parameter
of REGISTRY:START-REGISTRY.

If REGDEV is set to "true" in the environment, then the build will be
done with :REGISTRY-DEVELOPMENT in *FEATURES*. This builds with
(OPTIMIZE (SPEED 1) (SPACE 1) (DEBUG 3) (SAFETY 3)), which is very
slow in CCL, but easier to debug.

If REGISTRY_PRODUCTION is set to "true" in the environment, then the
build will be done with :REGISTRY-PRODUCTION in *FEATURES*. This
enables Twitter updates.

If ENABLE_EMAILS_TO_USERS is set to "true" in the environment, then
the variable *enable-email-to-users* will be set true after the
registry code is loaded, but before the server is started. This
enables periodic emails to users who have requested it.

Both Twitter updates and periodic emails can be toggled on the "Admin"
page.

Before you can start a registry server, you need to configure Elephant
for your version of the Berkeley Database (BDB). To do this:

    cd ~/lamsight/systems/elephant-1.0
    cp config.sexp my-config.sexp

Then, unless you're using BDB version 4.5, edit my-config.sexp, and
change 4.5 for your platform to the version you're using (4.6 or 4.7).

The first time you start the server, you'll have an empty database,
which will cause many of the pages to be blank. To load an initial
database, you can do the following on newmed-dev:

    (import-model-file "/usr/local/lamsight/anon-db-export-05-27-09.sexp")

If BDB isn't installed on your machine, you can download source from:

    http://www.oracle.com/technology/software/products/berkeley-db/

Make sure you compile and link for 64 bits, if you're using a 64-bit
CCL:

    cd /path/to/db-4.x.y/build_unix
    CFLAGS="-m64" LDFLAGS="-arch x86_64" ../dist/configure
    make
    sudo make install

I'm using version 4.5 on OS X. For a new installation, use the latest,
4.7.


## Guide to the Code Base

### Configuration parameters

### Core Data Models

### Application Plugins

### Site Events

