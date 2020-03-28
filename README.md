.TKBIBTEX (17 March 2000) 
- portable BibTeX editor and browser

# SYNOPSIS

```
tkbibtex [bibfile]
```

# DESCRIPTION

`tkbibtex`
is a portable editor and browser for BibTeX format file.  It is written in
Tcl/Tk and runs under Unix or Windows based wish interpreters.
It is inspired by, or modeled on, an ancient OpenWindows application called `bibcard`.
It also has a non-GUI command line mode which is useful for generating
publication lists.

`tkbibtex` supports 3 different windows:

1. A scrolling list of cite keys
which has a toolbar with File, Edit and Help menus.
The bibtex file can be given on the command line (it will be searched for along
the path given by the BIBPATH envariable), or opened via the File/Open
command.
The current bib file name is shown in the title bar, as well as the modification
status.
Double clicking on an entry brings up the selected citation in the
browser window.  The up and down arrow keys can also be used to move up and down the citation list.

2. The browser window shows all the fields associated with the selected entry
and its title bar shows the citation key.
The fields shown depend on the citation type which can be changed by the 
reference type menu button.
Required fields have a bold label while optional fields have an italic label.
String values are preceeded by a hash.

3. The search window provides a wide variety of searching modes.  The search
can cover all fields or be limited to a specific field.
The search can be further
constrained by reference type and year range by specifying either or both
a starting and ending (inclusive) year.
A list of matching cite keys is listed in this window, and once again
double clicking or up/down arrow keys can be used to browse this list using
the browser window.

# Features
The following are features I think are particularly cute.

* In the browser window you can right-click in a field and it will bring up
a menu of string constants.  I normally define strings for journal names.
The name of the bibtex string file that is used to build the menu can
be set in the `.tkbibtexrc`
file (eg. set strfile strings.bib).  It is searched for along the `BIBPATH`.

* The browser window has an `Annote' button which expands the annotation
window.  This scrolling text entry box allows for an arbitrary amount of 
freeform text to be stored
in the bibtex file in the `ANNOTE` field.  This is useful for storing commentary
or reviews of papers read.  This material does not appear in the bibliography
with any of the normal reference styles.

* Multiple selection is enabled in the main and search window citation lists.

* Edit/Copy copies the actual
bibtex source of selected citations to the clipboard.

* Edit/Paste pastes
bibtex source into the bibliography.

* Single-key shortcuts exist for most menu functions, see Help for details.

* Every time tkbibtex writes an output file it copies the existing bib file to
a unique name.  These build up over time and should be periodically culled.

# NON-GUI MODE
`tkbibtex`
also has a command line mode

`tkbibtex [switches] [bibfile]`

which can be used to select entries from the 
`bibfile ` according to various filtering options.

* `-type string`
apply search only to refernces of the specified type, ie. `article`
or `inproceedings`. This can also be a regexp, eg. `article!inproceedings`

* `-field string` apply search only to the specified field.

* `-search string` filter out all citations that match this string

* `-ys year` filter out articles with year greater than or equal to that specified (default 0)

* `-ye year` filter out articles with year less than or equal to that specified (default 9999)

* `-last nyears` filter out articles whose year falls within the last `nyears`

* `-exact` make all searches case sensitive

* `-count`
instead of outputting BibTeX entries to stdout, just count the matching
references.

* `-list`
instead of outputting BibTeX entries to stdout, provide a one
line summary of the form "key (type): title".

For example, to count the number of journal papers jointly authored 
with Foo before 1997, we could use the command line
```
% tkbibtex -count -ye 1997 -field author -search Foo   publist.bib
```

#FILES
At startup a file called `.tkbibtexrc`
in the user's home directory is sourced if it exists.

# ENVIRONMENT
`BIBPATH`
is a colon separated list of directories which will be searched for named
bib files.

# SEE ALSO
`bibtex`, `latex`, `tcl`, `tk`

# BUGS
The parser for the bibtex file format is simple minded and expects each
record to occupy a single line, ending with a `}` or `},`.  The last line
of each group must be a close brace on a line by itself.
This means that old, existing, bib files may need some manual editing 
before they will work with tkbibtex.

# AUTHOR
P.I. Corke (peter.i.corke@gmail.com)

