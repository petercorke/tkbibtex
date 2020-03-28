#! /bin/sh

# This is tkbibtex version 9, with some changes by Scott Otterson and
# Steve Juranich:
# * preserve newlines in the annote field.  SO: 6/15/01
# * added  bookfield to opt dict. so can use crossref field easily. SJ
# * removed/moved key bindings conflicting w/ emacs.  SO: 7/8/01
# * correct lyxpipe bug: now can set in .tkbibtexrc.  SO: 7/9/01
# * fix mouse pastes.  SO: 6/6/02
# * move code, annote, url fields to top.  SO: 6/6/02

# the next line restarts using wish \
exec wish $0 ${1+"$@"}

#---------------------------------------------------------------------------
# tkbibtex - a BibTeX file browser
# Copyright (c) 1997 Peter Corke	pic@cat.csiro.au
#  with major modifications by Guenter Milde (G.Milde@physik.tu-dresden.de)
# 
# Needs Tcl/Tk version 8.0 or later
#
# tkbibtex home page at
#    http://www.cat.csiro.au/cmst/staff/pic/tkbibtex.html
#
#  This is RELEASE 10 June 2002
#
set release 10
#
#---------------------------------------------------------------------------
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#---------------------------------------------------------------------------
# Contributors:
#	Guenter Milde (G.Milde@physik.tu-dresden.de)
#	John Fulton <John.Fulton@iname.com>
#	Thomas Henlich <henlich@mmers1.mw.tu-dresden.de>
#	Scott Otterson <scotto@u.washington.edu>
#
# COMMAND LINE SWITCHES:
# ~~~~~~~~~~~~~~~~~~~~~

#set usage {Usage: tkbibtex [switches] [bibfile]
set usage {Usage: tkbibtexS [switches] [bibfile]
   
   A browser/editor for BibTeX format bibliografy files
   
   * General switches:
   
   SWITCH	ARG	DESCRIPTION
   TYPE
   
   -debug int	0 = none, 1=moderate, 2=lots, ...
   -lyx  	string	name of pipe to connect to LyX with
   
   * For non-interactive searching mode (entered if any of these switches given)
   
   -search	string	search string (defaults to all)
   -type	string	reference type to search (defaults to all)
   -field	string	field to search (defaults to all)
   -ys		int	starting year (inclusive) (defaults to 0)
   -ye		int	ending year (inclusive) (defaults to 9999)
   -last	int	ending year is this year, starting year is specified
                        number of years earlier
   -exact		case sensitive search
   -case		case sensitive search, by default will list the 
			BibTeX source of the matching entries to stdout, but
   -key         will list the matching cite keys (one per line)
   -list        will display a one line summary (key, type, title) per match
   -count	will output number of matches only, not BibTeX source

   -select     citekeys will output the BibTex entries for the specified
			list of citekeys, separated by whitespace.  Use
			quotes so that shell treats key list as one argument.

Copyright (c) Peter Corke 1997-2000

   }

## user alterable inits, you may set/override these also in a file ~/.tkbibtexrc

#	default for ignore case check button in search window
set search_ignore_case 1
#       which fields shall have a folding multiple line entry?
set bigfields "annote title abstract"
#	number of lines in big fields
set title_lines_big     4
set annote_lines_big	10
set abstract_lines_big	10
set delete_confirm	true

# cite_grouping, controls how a list of references is pushed to LyX
#	false		[ref1][ref2][ref3]
#	true		[ref1,ref2,ref3]
set cite_grouping	true

# citekey_lower, controls whether all cite keys are made lower case. bibtex
# ignores case, so this is useful to detect duplicates.
set citekey_lower	false
#	name of strings file, searched for along BIBPATH
set strfile "strings.bib"
#	initial height of references (main) window (in chars)
set refs_height 30
#	initial width of references window (in chars)
set refs_width 30
# initial width of entries in browse window (in chars)
set entrywidth 75
# maximal number of backup copies: 0 = no backups, -1 unlimited
set backup_max 9
# default for new entries
set reftype_default "article"
# where lyx pipe is
# SO: set before reading .tkbibtexrc so can be overwritten (this was a bug)
set LyXpipe $env(HOME)/.lyxpipe

# load user's init file if it exists
if {[file exists "~/.tkbibtexrc"]} {
   source "~/.tkbibtexrc"
}

# get BIBPATH environment variable if it exists
if {[info exists env(BIBPATH)]} {
   set bibpath $env(BIBPATH)
   set bibpathlist [split $bibpath :]
} else {
   set bibpathlist ""
}

## initializations, modify with caution
set curkey ""      ;# keyword of the entry shown in the .browse window
                    # or "" if there is no .browse window
set debug 0        ;# verbosity level 0,...,3  for debugging 
set bibchanged 0		
trace variable bibchanged w bibchange_proc
set citelist ""
set window_state ""
set curreftype ""
set fname "<noname>"
set global_print_cmd "|lpr"

foreach i $bigfields {set ${i}_big 0} ;# big_lentries folded to one line

# initial values of command params that may be set by command line switches
set arg_ys 0
set arg_ye 9999
set arg_search 9999
set arg_reftype "All"
set arg_field  "All"
set arg_search  ".*"
set arg_ignorecase   1
set arg_count   0
set arg_key   0
set arg_list   0
set arg_select {}
set command_line_mode 0
# SO: don't set here, as .tkbibtexrc has been read and don't want to overwrite
#set LyXpipe $env(HOME)/.lyxpipe

# global bindings
bind Button <Up> {tkTabToWindow [tk_focusPrev %W]}
bind Button <Down> {tkTabToWindow [tk_focusNext %W]}
bind Button <Left> {tkTabToWindow [tk_focusPrev %W]}
bind Button <Right> {tkTabToWindow [tk_focusNext %W]}

# SO: remove or move bindings that conflict w/ basic emacs keybindings
bind all <F1> {briefhelp Keys}
# what is this??
#bind all <Control-d> {edit_item duplicate}
bind all <Control-r> {edit_item rename}
#bind all <Control-n> {edit_item new}
bind all <Meta-n> {edit_item new}
#bind all <Control-f> {finditem}
bind all <Control-s> {finditem}
#bind all <Control-s> {savebib}
bind all <Meta-s> {savebib}
bind all <Control-q> {quit}
bind all <F2> {savebib}
bind all <Shift-F2> {saveasbib}
bind all <F3> {openbib}
bind all <Shift-F3> {closebib}


#---------------------------- DEFINES -------------------------------------
# define BibTeX reference types and fields, see btxdoc.[tex/bib] for 
# documentation

# common month names for the month menu
set months {jan feb mar apr may jun jul aug sep oct nov dec}

#---------------------------------------------------------------------
# The following lists tailor the operation of tkbibtex. 
# 1. to add a new reference type, say a BIGBOOK type, add the name (in l.c.)
#    to the list "alltypes"
# 2. to add a new field place the name in the lists "allfields" and:
# 3. to make it mandatory for a type add that field name to the
#    list  req(reftype).
# 4. to make it optional for a type add that field name to the
#    list  opt(reftype).
# 5. to make it an additional field (for all types) add that field name 
#    to the list ign (ignored fields)
#---------------------------------------------------------------------

# list of all fields
set allfields {address author booktitle chapter edition \
     editor howpublished institution journal month number \
     organization pages publisher school series title type volume \
  year note code url crossref annote abstract}

# list of all reference types
set alltypes {article book booklet inbook incollection \
     inproceedings manual mastersthesis misc phdthesis proceedings \
  techreport unpublished}

# list of additional fields, ignored by the standard BibTeX styles
# SO: move code, annote, url to top since I'm more likely to read these.
set ign {annote code url crossref abstract}

# lists of required and optional fields for each reference type
set req(article) {author title journal year}
set opt(article) {volume number pages month note}

set req(book) {author title publisher year};# requires author OR editor
set opt(book) {editor volume number series address edition month note}

set req(booklet) {title}
set opt(booklet) {author howpublished address month year note}

set req(inbook) {author title chapter pages publisher year}
                #(requires author or editor)
set opt(inbook) {editor volume series address edition month note}

set req(incollection) {author title booktitle publisher year}
set opt(incollection) {editor volume number series type chapter \
                       pages address edition month note}

set req(inproceedings) {author title booktitle year}
set opt(inproceedings) {editor pages organization publisher address month note}

set req(manual) {title}
set opt(manual) {author organization address edition month year note}

set req(misc) {}
set opt(misc) {title author howpublished month year note}

set req(mastersthesis) {author title school year}
set opt(mastersthesis) {address month note}

set req(phdthesis) {author title school year}
set opt(phdthesis) {address month note}

set req(proceedings) {title year}
# I added the bookfield to the opt dictionary here so that we can use
# the crossref field more easily.
set opt(proceedings) {editor publisher organization address month booktitle note}

set req(techreport) {author title institution year}
set opt(techreport) {type number address month note}

set req(unpublished) {author title note}
set opt(unpublished) {month year}

#-------------------------- Datastructures -----------------------------
#
# Datastructures are somewhat limited in Tcl.  The two main ones are:
#
#	citelist	a list of all citations in the order they
#			 are displayed in the main listbox
#	REFLIST		an array in which the first index is the citation key,
#			 and the second index is the field.  Thus
#			 REFLIST(Corke87b,type) = "article"
#			 REFLIST(Corke87b,title) = "Yet another paper about..."
#

#----------------------- BibTeX format file i/o -----------------------------

# return full path name for specified file along BIBPATH
proc find_on_path {fname} {
   global	bibpathlist
   
   if {[file exists $fname]} {
         return $fname
   }
   	
   foreach p $bibpathlist {
      if {[file exists $p/$fname]} {
         set fname $p/$fname
         return $fname
      }
   }
   return {}
}

#
# parse the BibTeX format file and update the multidimensional array REFLIST.
#
proc bibparse {fname} {
   global citelist errmsg
   
   showmsg "loading $fname"
   . config -cursor watch
   trace vdelete REFLIST w REFLISTchange_proc
   set dupcount [bibparse_f $fname]
   trace variable REFLIST w REFLISTchange_proc
   set REFLIST_update 0
   update_citelistbox
   
   . config -cursor ""
   set count [llength $citelist]
   if {$dupcount == 0} {
      showmsg "$count records"
   } else {
      showmsg "$count records ($dupcount duplicates)"
   }
   testoutput 1 "$count records ($dupcount duplicates)"
   if {$errmsg != ""} {
      tk_messageBox -icon warning -type ok -message $errmsg
      set errmsg ""
   }

}

# --------  new parsing-algorithm by GM (rparse.tcl) --------------

set preamblelist ""
set errmsg ""

# core file parser

proc bibparse_f {fname} {

   global errmsg

	puts stderr "Loading from $fname"
   set f [open $fname r]
   
   testoutput 1 "loading $fname"
   showmsg  "loading $fname"

   set buffer [read $f]
   close $f

   testoutput 1 "parsing $fname"
   showmsg  "parsing $fname"

   return [bibparse_s $buffer]
}
   
proc bibparse_s {buffer} {
   # transform, to get a string that can be parsed by tcl-routines:

   # We have to cheat to get tcl parsing a bibtex file: BibTeX uses special 
   # characters to separate entries and fields. But inside a {string}
   # or a "string" they are escaped. This escaping is not regarded by "split"
   # or "regexp", but it is implemented in Tcl's list functions, 
   # however assuming a space as separating character
      
   # 1. escape backslashes (to prevent substitution of LaTeX-commands as 
   # there is one round of backslash substitution when the list element is not
   # enclosed in braces) and quote whitespace

   set new_entry 0
   set count 0
   set dupcount 0
   set catch_msg ""

   regsub -all {\\} $buffer {\\\\} buffer
   regsub -all "\[\t\n ]+" $buffer {\ } buffer

   # 2. put in additional whitespace where we want a separation
   #    '@' separates entries, ',' separates fields,
   # 3. braces and quotes must be surrounded by whitespace, 
   #    otherwise list commands will produce errors

   regsub -all "\[@\{,]" $buffer { &} buffer
   regsub -all "\[\}]" $buffer {& } buffer
   regsub -all {"} $buffer { & } buffer ;# comment " only for syntax color
    
   testoutput 3 $buffer

   # now look at the entries
   foreach element $buffer {

      testoutput 3  "<$element>"
   
      # the first element after a reftype contains the fields
      if {$new_entry} {
         parse_fields $element $reftype
         if {$catch_msg != ""} {
            append errmsg "$catch_msg \n\
               The following record could not be parsed:\n  \
               <[ruecktrafo $element]> \n\n\
               You might have to fix your *.bib file! \
               (probabely use of \"quotes\" \
               to enclose fields with umlauts) \n\n"
         }   
         set new_entry 0
         set catch_msg ""
         continue
      }
      # look for reftype
      if [regexp {^@([a-zA-Z]+) ?(\(?)} $element match reftype parantheses] {
         set reftype [string tolower $reftype]
         incr count
         if {$count%10 == 0} { # show progress every tenth parsed entry
            showmsg "parsing: $count records"
         }
         testoutput 1  "\nNew entry:"
         if {$parantheses == "("} {
           append errmsg "cannot handle entry in parantheses (yet): $element\n"
           # Baustelle 
           # diese Klausel kann weg, wenn Klammern richtig behandelt werden
         } else { 
            set new_entry 1
         }    
         continue
      }
      # somethig else: warn user, as this will not be written back to file
      testoutput 2 "ignored: <$element>"
      if [regexp {[a-zA-Z]+} $element] {
         append errmsg "ignored text: [ruecktrafo $element]\n"
      }
   }
   return $dupcount
}


proc parse_fields {fields reftype} {

   global  preamblelist strlist citelist REFLIST errmsg citekey_lower
   upvar dupcount dupcount
   upvar catch_msg catch_msg
   set pre_types {preamble string comment}
   set fieldtext 0
   set macro ""

   testoutput 3 $fields

   # store preambles, strings and comments to write them back 
   # when saving the file
   if {[lsearch $pre_types $reftype] >= 0 } {
      lappend preamblelist "@$reftype \{[ruecktrafo $fields]\}"
      testoutput 1  "pre-stuff: [lindex $preamblelist end]"
      if {$reftype == "string"} {
         # strings will be parsed and put to the stringlist although
         set keyw ""   ;# a string definiton has no keyword
      } else {      
         return 
      }
   }

   # process the fields
   # use catch, as the list might contain syntactic errors
   catch {
      foreach item $fields {
         testoutput 2  "<$item>"
         
         # first field is keyword (except for strings)
         if {![info exists keyw]} {
            set keyw [string trim $item]
	    if {$citekey_lower} {
		    set keyw [string tolower $keyw]
	    }
            # put in the Reflist
            set REFLIST($keyw,reftype) $reftype
            # if not already in the citelist, append it
            if {[lsearch -exact $citelist $keyw] < 0} {
               lappend citelist $keyw
            } else {
               # Baustelle: fragen: rename overwrite skip?
               set r [tk_dialog .duplicate {Duplicate!} \
                  "Cite key <$keyw> already exists" \
                  warning 0 {Skip} {Overwrite}]
               switch $r {
                  0 {incr dupcount; return}
                  1 {incr dupcount}
               }
            }
            testoutput 1  "$reftype $keyw"
            continue
         }
         
         # item is <"text"> or <{text}>
         if {$fieldtext} {
            set REFLIST($keyw,$fieldname) [ruecktrafo $item]
            set fieldtext 0
            testoutput 1 "  $fieldname   $REFLIST($keyw,$fieldname)"
            continue
         }
         
         # count for concatenations
         if [info exists concat] {
            testoutput 3 "concat = <$concat>"
            append REFLIST($keyw,$fieldname) "\{[ruecktrafo $item]\}"
            unset concat
            testoutput 1 "  $fieldname   $REFLIST($keyw,$fieldname)"
            continue
         }
         
         # item of the form <?,?fieldname = ?macro? ?#?>
         if [regexp {^,? ?([^ ]+) ?= ?(.*)} \
           $item match fieldname macro] {
            set fieldname [string tolower $fieldname]
            if {$macro != ""} {
               if [regexp {# ?$} $macro] {
                  set concat "#"
               }
               set REFLIST($keyw,$fieldname) "# $macro"
               testoutput 1 "  $fieldname  $REFLIST($keyw,$fieldname)"
               set macro ""
            } else {
               set fieldtext 1
            }
            continue
         }
         # concatenations < # ?macro? ?#?>
         if [regexp {^ ?(#.*)} $item match concat] {
            # if the first part is not a macro, put braces around
            if {![regexp {^#} $REFLIST($keyw,$fieldname)]} {
               set REFLIST($keyw,$fieldname) "#\{$REFLIST($keyw,$fieldname)\}"
            }
            append REFLIST($keyw,$fieldname) " $concat"
            testoutput 1 "  $fieldname  $REFLIST($keyw,$fieldname)"
            if {![regexp {# ?$} $concat]} {
               unset concat
            }
            continue
         }
         
         if {![regexp {^ *,? *$} $item]} {
            append errmsg "cannot parse <$item> \n"
         }
         
      } ;# ende {foreach {$fields}}...
   } catch_msg;# ende catch {foreach ....}

   if {$reftype == "string"} {
      set strlist($fieldname)  $REFLIST($keyw,$fieldname)
      unset REFLIST($keyw,$fieldname)
   }
   if [info exists keyw] {unset keyw}
}
# ende proc parse fields


proc ruecktrafo {string} {
   # 1. change back spaces
   regsub -all "(\}) " $string {\1} string
   regsub -all " (\[@\{,])" $string {\1} string
   regsub -all { (") } $string {\1} string      ;# "only for syntax color

   # Test what kind of string it is:
   # if it was in quotes, it will have spaces around -> remove
   # if the string was in braces  -> dequote it
   if {[regsub {^ (.*) $} $string {\1} string] == 0} {
      regsub -all {(\\)?\\} $string {\1} string
      } 
   return $string
}
 
# --------- end of stuff for the new parsing algorithm by GM ---------

proc update_citelistbox {} {
   global citelist
   
   # cream the old listbox contents
   .refs.list delete 0 end
   
   # now reinsert all items in citelist order
   foreach key $citelist {
      .refs.list insert end $key
   }
}

proc append_to_citelist {newkey} {
   global bibchanged citelist
   
   .refs.list insert end $newkey
   lappend citelist $newkey
   # note that we've updated the bib
   incr bibchanged		
}


# writebib	package
#
#	writebib citelist filename
#
#		writes specified citations to the given filename
#
#	writebib2str citelist
#
#		writes specified citations to the global buffer writebib_str
#		and also returns this string
#

proc writebib {citelist fname} {
   global debug
   
   showmsg "writing $fname"
   if {$debug} {
      puts "writing $fname"
   }
   . config -cursor watch
   set f [open $fname w]
   
   writebib2 $citelist fwrite $f
   close $f
   
   . config -cursor ""
   showmsg "[llength $citelist] entries written"
}

proc writebib2str {citelist} {
   global	writebib_str
   
   set writebib_str ""
   writebib2 $citelist swrite 0
   
   return $writebib_str
}

proc writebib2 {citelist wproc arg} {
   global REFLIST 
   
   foreach k $citelist {
      set rt $REFLIST($k,reftype)

      $wproc $arg "@$rt\{$k"
      
      set res [array get REFLIST "$k,*"]
      # now have a list comprising pairs of list elements; the REFLIST 
      # subscript then the value
      set nfound 0
      
      foreach {key val} $res {
         regexp {,([a-z]+)} $key matchall lastkey
         if {($lastkey != "reftype") && ($val != "")} {
            testoutput 1 "<$key $val$>"
            $wproc $arg ",\n"
            if {[regexp ^# $val]} {
               set val [string trimleft $val "#"]
               $wproc $arg "	$lastkey = $val"
            } else {
                if {($wproc == "fwrite") && ($lastkey == "annote")} {
                    # "\n" -> "^^": kludge to allow multi-line annote
                    regsub -all "\[\n]" $val "\^\^" val
                }
                $wproc $arg "	$lastkey = \{$val\}"
            }
         }
      }
      # put end of record brace
      $wproc $arg "\n\}\n\n"
   }
}

proc fwrite {f str} {
   puts -nonewline $f $str
}

proc swrite {f str} {
   global	writebib_str
   
   append writebib_str $str
}


#----------------------- BROWSE WINDOW -----------------------------------

# create the data entry window (.browse) with reftype menu and navigation bar
# (the fields will be inserted by set_browsebox)
proc create_browse {} {
   global alltypes curreftype reftype_default
   
   toplevel .browse
   build_stringmenu
   bind .browse <Destroy> {
     if {"%W" == ".browse"} {
         set curkey ""
         trace vdelete curreftype w reftypechange_proc
     }
   }
	bind .browse <Unmap> {wm_statechange %W}
	bind .browse <Map> {wm_statechange %W}
   # Reference type is a menu button whose current label shows the 
   # reference type.  Clicking brings up a radiobutton menu that allows
   # the type to change, and the browse box will reconfigure appropriately
   frame .browse.reftype
   label .browse.reftype.l -text "Reference type:" -width 15 -anchor e
   menubutton .browse.reftype.m -menu .browse.reftype.m.menu -width 20 \
      -relief raised -anchor w -text $reftype_default
   menu .browse.reftype.m.menu
   foreach i $alltypes {
      .browse.reftype.m.menu add radiobutton -label $i -variable curreftype \
         -value $i 
   }
   pack .browse.reftype.l .browse.reftype.m -side left -anchor w
   pack .browse.reftype  -expand 1 -anchor w
   
   # look for changes in the global curreftype
   trace variable curreftype w reftypechange_proc
   
   # a bottom line with helpful buttons:
   frame .browse.navigate
   button .browse.navigate.first -text {|<  First} -underline 4\
     -command {browseitem first .refs.list}
   button .browse.navigate.prev -text {<  Previous} -underline 3\
     -command {browseitem previous .refs.list}
   button .browse.navigate.next -text {Next  >} -underline 0\
     -command {browseitem next .refs.list}
   button .browse.navigate.last -text {Last  >|}  -underline 0\
     -command {browseitem last .refs.list}
   button .browse.navigate.close -text Close -width 10 -underline 0\
     -command {destroy .browse}
   button .browse.navigate.help -text Help -width 10 -underline 0\
     -command {briefhelp Browse}
   pack .browse.navigate.first .browse.navigate.prev .browse.navigate.next\
     .browse.navigate.last -side left -fill x -expand 1
   pack .browse.navigate.help .browse.navigate.close -side right
   pack .browse.navigate -side bottom -fill x
   bind .browse.navigate 1 {focus .browse.navigate}
   
   # bind the Alt + underlined keys
   bind .browse <Alt-Key-f> {browseitem first .refs.list; break}
   bind .browse <Alt-Key-p> {browseitem previous .refs.list; break}
   bind .browse <Alt-Key-n> {browseitem next .refs.list; break}
   bind .browse <Alt-Key-l> {browseitem last .refs.list; break}
   bind .browse <Alt-Key-c> {destroy .browse; break}
   bind .browse <Alt-Key-h> {briefhelp Browse; break}
}


# browse the specified cite key, optional arg "-focus" controls
#  whether the first item of .browse is focused on 
proc browse {name args} {
   global REFLIST allfields curkey curreftype curfields \
          bibchanged debug bigfields fname
   
   # ignore stupid keys 
   #   if {$name == ""} {return} ;# does this happen?
   
   # unset empty array elements of last browsed entry
   if {$curkey != ""} {
      trace vdelete REFLIST w REFLISTchange_proc
      foreach el [array names REFLIST $curkey,*] {
        if {$REFLIST($el) == ""} {unset REFLIST($el)}
      }
      trace vdelete REFLIST w REFLISTchange_proc
   }
  
   # set curkey to new value
   set curkey $name   

   # create browse window if need be and add the labeled entries for curreftype
   if {[winfo exists .browse] == 0} {
      create_browse
      set_browsebox $REFLIST($name,reftype)
   } 

   # configure the browse box with fields for this entry
   if {$curreftype != $REFLIST($name,reftype)} {
      # update curreftype: this calls set_browsebox and a rerun of browse 
      # via trace/curreftypechange_proc
      set curreftype $REFLIST($name,reftype)
      return
   } else {
      # delete non canonical fields (not predefined for this reference type)
      foreach i [lrange [pack slaves .browse] 2 end] {
         if [regexp {sep} $i] {continue}
         if {[lsearch $curfields [string range $i 8 end]] < 0} {destroy $i}
      }
   }
   # add nan canonical fields (not predefined but present in REFLIST)
   foreach el [array names REFLIST $name,*] {
      if [regexp {,([a-z]+)} $el matchall fieldname] {
         if {$fieldname == "reftype"} {continue}
         if {[lsearch $curfields $fieldname] < 0} {
            lentry $fieldname
         }
      }
   }
   
   # attach the entry to the browse box by textvariables, 
   # do special actions for big_lentry boxes
   foreach entry [lrange [pack slaves .browse] 2 end] {
      if [regexp {sep} $entry] {continue}
      set field [string range $entry 8 end]
      if {[lsearch $bigfields $field] >= 0} {
         .browse.$field.e delete 1.0 end
         if [info exists REFLIST($name,$field)] {
            # for display, subs "^^" with newlines: kludge for multiline annote  
            set withnewlines $REFLIST($name,$field)
            regsub -all {\^\^} $withnewlines "\n" withnewlines
            .browse.$field.e insert 1.0 $withnewlines
         }
      } else {
         trace vdelete REFLIST w REFLISTchange_proc
         set in_reflist [info exists REFLIST($name,$field)]
         # now link entry box to data using textvariable
         .browse.$field.e config -textvariable REFLIST($name,$field)
         if {!$in_reflist} {
            .browse.$field.e delete 0 end
         }
         trace variable REFLIST w REFLISTchange_proc
      }
   }
   
   # put cite key into the titlebar
   wm title .browse "\[$fname\] $name"

   # set focus to first entry if called with option -focus
   # -force, as this is another toplevel window
   if { [lindex $args 0]  == "-focus"} {
      focus -force .browse.[lindex $curfields 0].e
   }    
}

# configure the browse window fields for specified reference type
proc set_browsebox {reftype} {
   global req opt ign

   # clear all old fields, (0st and 1st slave are reftype and navigate)
   foreach i [lrange [pack slaves .browse] 2 end] {
   #  pack forget $i ;
   # we better destroy and make anew, as this keeps the
   # right order for keybord traversal
      destroy $i
   }

   # add the new fields in right order, separate types with black lines
   foreach i $req($reftype) {
     lentry $i
   }
   frame .browse.sep1 -height 3 -background black
   pack .browse.sep1 -fill x -expand 1

   foreach i $opt($reftype) {
     lentry $i
   }
   frame .browse.sep2 -height 3 -background black
   pack .browse.sep2 -fill x -expand 1
   
   foreach i $ign {
     lentry $i
   }

}

# create a labelled entry box, a frame holding a label and an entry 
# (with multiple lines if name is mentionend in global $bigfields)
proc lentry {name} {
   global bigfields entrywidth months

   if {[winfo exists .browse.$name] == 0} {
      frame .browse.$name
      if {[lsearch $bigfields $name] >= 0} {
         button .browse.$name.l -text "[string toupper $name]:" -width 14 \
           -anchor e -default disabled -bd 1 -padx 2 -pady 0 \
           -highlightthickness 0 -takefocus 0 -command "fold_button {$name}"  
         text .browse.$name.e -yscrollcommand ".browse.$name.s set" -height 1 \
            -width 20
         bind .browse.$name.e <KeyRelease> "update_biglentry $name"
         bind .browse.$name.e <ButtonRelease> "update_biglentry $name"
         bind .browse.$name.e <Tab> {tkTabToWindow [tk_focusNext %W]; break}
         bind .browse.$name.e <Shift-Tab> {
                                     tkTabToWindow [tk_focusPrev %W]; break}
         bind .browse.$name.e <Up> {tkTextfieldUp %W}
         bind .browse.$name.e <Down> {tkTextfieldDown %W}
         # scrollbar will be displayed by proc fold_button if entry unfolded
         scrollbar .browse.$name.s -takefocus 0 -command ".browse.$name.e yview"
         # SO: default bind ordering is window-first but mouse paste events are
         # at class (Text) level.  With that ordering, update_biglenentry
         # is called before the paste has been written to the text widget:
         # Rearrange so paste happens before update_biglenentry is called.
         eval bindtags .browse.$name.e "\{Text .browse.$name.e all\}"
      } else {
         label .browse.$name.l -text "[string toupper $name]:" \
                               -width 15 -anchor e
         entry .browse.$name.e -width $entrywidth -insertofftime 0 
         bind .browse.$name.e <Up> {tkTabToWindow [tk_focusPrev %W]}
         bind .browse.$name.e <Down> {tkTabToWindow [tk_focusNext %W]}
         bind .browse.$name.e <Return> {tkTabToWindow [tk_focusNext %W]}
      }
      pack .browse.$name.l -side left
      pack .browse.$name.e -side left -fill x -expand 1
   }
   pack .browse.$name -fill x -expand 1

   # some special cases
   switch $name {
   month  {
	# bind right mouse button to string popup menu
	menu .browse.month.menu
	foreach i $months {
		.browse.month.menu add radiobutton -label $i -value #$i \
			 -command "month_menu_proc $i"
	}
	bind .browse.$name.e <Button-3> "month_popup %W %X %Y"
    }
   crossref {
	bind .browse.$name.e <Button-3> "crossrefGoto %W"
    }
   default {
	# bind right mouse button to string popup menu
	bind .browse.$name.e <Button-3> "string_popup %W %X %Y"
    }
   }

}

proc crossrefGoto {w} {
	set newkey [$w get]
	puts "going to $newkey"
	browse $newkey
}

proc fold_button {name} {
   global  ${name}_big ${name}_lines_big
   
   if [set ${name}_big] {
      .browse.$name.e config -height 1 -wrap none
      pack forget .browse.$name.s 
      set ${name}_big 0
   } else {
      .browse.$name.e config -height [set ${name}_lines_big] -wrap word
      pack .browse.$name.s -side right -fill y
      set ${name}_big 1
   }
}

# if cursor is on first line: move to previous window,
# otherwise the default binding works (move up one line)
# to be bound to <Key-Up> in a textfield inside a formular
proc tkTextfieldUp {w} {
   if {[$w index insert] == [tkTextUpDownLine $w -1]} {
      tkTabToWindow [tk_focusPrev $w]
      return break
   }
}

# if cursor is on last line: move to next window,
# otherwise the default binding works (move down one line)
# to be bound to <Key-Down> in a textfield inside a formular
proc tkTextfieldDown {w} {
   if {[$w index insert] == [tkTextUpDownLine $w 1]} {
      tkTabToWindow [tk_focusNext $w]
      return break  
   }
}


#invoked on right mouse button click in a month lentry box
proc month_popup {w x y} {
   global month_select
   
puts "<$x $y>"
   # pop-up the string menu
   tk_popup .browse.month.menu $x $y
   tkwait variable month_select
   $w insert insert $month_select
}

proc month_menu_proc {which} {
   global	month_select
   
puts "in month_menu_proc which = $which"
   set month_select "#$which"
}


#invoked on right mouse button click in a lentry box
proc string_popup {w x y} {
   global	string_select
   
   # pop-up the string menu
   tk_popup .browse.strmenu $x $y
   tkwait variable string_select
   $w insert insert $string_select

   # patch by Thomas Henlich henlich@mmers1.mw.tu-dresden.de
   set string_select ""
}

proc strmenu_proc {which} {
   global	string_select
   
   set string_select "#$which"
}

proc build_stringmenu {} {
   global	strlist
   
   menu .browse.strmenu
   foreach i [array names strlist] {
      .browse.strmenu add command -label $strlist($i) -command "strmenu_proc $i"
   }
}

# if global variable curreftype has changed: 
# invoke set_browsebox() and update REFLIST
proc reftypechange_proc {name el op} {
   global curreftype curkey REFLIST debug curfields req opt ign 
   
   set curfields [concat $req($curreftype) $opt($curreftype) $ign]
   .browse.reftype.m config -text $curreftype
   if {$REFLIST($curkey,reftype) != $curreftype} {
      set REFLIST($curkey,reftype) $curreftype
   }
   set_browsebox $curreftype
   browse $curkey
   testoutput 1 "reftype changing to $curreftype"
}

# as we cannot bind a textvariable to a text (only to an entry)
# make sure that the big fields displayed in the browse box are updated
# in the REFLIST.
# bound to <KeyRelease>, <ButtonRelease>
proc update_biglentry {fieldname} {
   global REFLIST curkey bibchanged
   set fieldtext [.browse.$fieldname.e get "1.0" end-1c]
   if {[info exists REFLIST($curkey,$fieldname)]} {
      # if fieldtext prev existed & has changed
      if {$fieldtext != $REFLIST($curkey,$fieldname)} {
         set REFLIST($curkey,$fieldname) $fieldtext
#         incr bibchanged # done by trace on REFLIST
      }
   } else {
      # no fieldtext in REFLIST, check if now finite
      if {$fieldtext != ""} {
         set REFLIST($curkey,$fieldname) $fieldtext
#         incr bibchanged
      }
   }
}

# if the bib data has changed flag that in the main title bar
proc REFLISTchange_proc {name el op} {
   global bibchanged 
   
   incr bibchanged
   #puts stdout "REFLIST $el changed, $bibchanged"
}

#
# if any widget changes the bibliography in any way it increments the global
# bibchanged.  This trace handler then takes the necessary action
#
proc bibchange_proc {name el op} {
   global bibchanged fname
   
   if {$bibchanged == 0} {
      wm title . [file tail $fname]
   } elseif {$bibchanged == 1} {
      wm title . "[file tail $fname] (mod)"
   }
   # clear the search window listbox
   if {[winfo exists .find]} {
      .find.list.lb delete 0 end
   }
}

#----------------------- SEARCH WINDOW -----------------------------------

# invoked to do the actual search
proc searchbib {} {
   global find_field searchstring citelist REFLIST  search_ignore_case
   global find_reftype find_startyear find_endyear
   
   . config -cursor watch
   
   # clear out the old entries in the results listbox
   .find.list.lb delete 0 end
   .find.m configure -text ""
   
   set found [do_search $searchstring $find_reftype $find_field $find_startyear $find_endyear $search_ignore_case]
   foreach f $found {
      .find.list.lb insert end $f
   }
   
   # display how many hits
   set nfound [llength $found]
   .find.m configure -text [format "%d match(es) found" $nfound]
   
   # put selection on first line of list box for next/prev buttons
   .find.list.lb selection set 0
   focus .find.list.lb

   . config -cursor ""
   
}

proc do_search {searchstring find_reftype find_field find_startyear find_endyear search_ignore_case} {
   global REFLIST citelist debug
   
   # convert search field to upper case and extract relevant (or all) fields
   if {$find_field != "All"} {
      set el $find_field
   }
   if {$search_ignore_case} {
      set search {regexp -nocase}
   } else {
      set search {regexp}
   }
   
   if {$debug} {
      puts "startyear: $find_startyear"
      puts "endyear: $find_endyear"
      puts "reftype: $find_reftype"
      puts "fieldtype: $find_field"
      puts "search : $searchstring"
      puts "LC test: $search"
   }
   
   
   # now we have a list comprising pairs of list elements:
   #  eg.  { Corke96a,TITLE   "blah blah blah" }
   set found {}
   
   foreach ref $citelist {
      # screen for reference type (which can be a regexp)
      if {$find_reftype != "All"} {
         if {[regexp -nocase $find_reftype $REFLIST($ref,reftype) mv] == 0} {
            continue;
         }
         if {$mv != $REFLIST($ref,reftype)} {
            continue
         }
      }
      
      # screen for year if its specified
      if {($find_startyear != "") || ($find_endyear != "")} {
         if {[info exists REFLIST($ref,year)]} {
            set year $REFLIST($ref,year)
            if {($find_endyear != "") && ($year > $find_endyear)} {
               continue
            }
            if {($find_startyear != "") && ($year < $find_startyear)} {
               continue
            }
         }
      }
      
      # now check particular field
      if {$find_field == "All"} {
         set res [array get REFLIST "$ref,*"]
         foreach {key val} $res {
            if {[eval $search $searchstring {$val}]} {
               #found substring
               lappend found $ref
               break
            }
         }
      } else {
         if {![info exists REFLIST($ref,$el)]} {
            continue
         }
         set w $REFLIST($ref,$el)
         testoutput 1 "$w"
         if {[eval $search $searchstring {$w}]} {
            #found substring
            lappend found $ref
         }
      }
   }
   return $found
}

# create the find/search box
proc create_find {} {
   global allfields alltypes find_field searchstring  search_ignore_case
   global find_reftype find_startyear find_endyear
   
   toplevel .find
   bind .find <Destroy> {
      destroy .find
      trace vdelete find_field w find_fieldchange_proc
      trace vdelete find_reftype w find_reftypechange_proc
   }
	bind .find <Unmap> {wm_statechange %W}
	bind .find <Map> {wm_statechange %W}
   #explaining Header
   wm title .find "find entries using regular expressions"
   
   ######################################################################
   # create the search entry boxes
   #	search string
   #	year range
   #	reference type
   
   ######################################################################
   # search string and ignore case button
   frame .find.str
   label .find.str.l -text "Search for:" -width 12 -anchor e -borderwidth 1
   entry .find.str.e -textvariable searchstring
   bind .find.str.e <Return> {searchbib}
   checkbutton .find.str.c -variable search_ignore_case -text "Ignore case"
   pack .find.str.l -side left
   pack .find.str.e  -fill x -expand 1 -side left
   pack .find.str.c  -expand 1 -side left
   pack .find.str -fill x -expand 1 -pady 2m
   
   ######################################################################
   # create a menu button for search type
   frame .find.field
   label .find.field.l -text "Field:" -width 12 -anchor e
   trace vdelete find_reftype w find_reftypechange_proc;# just in case still traced
   set find_field All
   
   menubutton .find.field.mb -menu .find.field.mb.menu -relief raised\
      -text $find_field
   
   menu .find.field.mb.menu
   .find.field.mb.menu add radiobutton -label All -variable find_field -value All 
   foreach i $allfields {
      .find.field.mb.menu add radiobutton -label $i -variable find_field -value $i
   }
   trace variable find_field w find_fieldchange_proc
   
   pack .find.field.l -side left
   pack .find.field.mb -fill x
   pack  .find.field -fill x
   
   ######################################################################
   # create a menu button for all reference types + All
   set find_reftype All
   frame .find.reftype
   label .find.reftype.l -text "Ref Type:" -width 12 -anchor e
   
   menubutton .find.reftype.mb -menu .find.reftype.mb.menu -relief raised -text $find_reftype
   menu .find.reftype.mb.menu
   .find.reftype.mb.menu add radiobutton -label All -variable find_reftype -value All
   foreach i $alltypes {
      .find.reftype.mb.menu add radiobutton -label $i -variable find_reftype -value $i
   }
   trace variable find_reftype w find_reftypechange_proc
   pack .find.reftype.l -side left -fill x
   pack .find.reftype.mb -fill x -expand 1
   pack .find.reftype -fill x -expand 1
   
   
   ######################################################################
   frame .find.years
   label .find.years.l -text "Years:" -width 12 -anchor e
   pack .find.years.l -side left
   entry .find.years.start -textvariable find_startyear 
   entry .find.years.end -textvariable find_endyear
   pack .find.years.l .find.years.start .find.years.end -side left -fill x -expand 1
   pack .find.years  -expand 1
   
   ######################################################################
   # the search list box
   frame .find.list
   listbox .find.list.lb -yscrollcommand ".find.list.sb set" -selectmode extended
   scrollbar .find.list.sb -command ".find.list.lb yview"
   pack .find.list.sb -side right -fill y
   pack .find.list.lb -side left -fill x -expand 1
   pack .find.list -fill x -expand 1
   
   bind .find.list.lb <Double-Button-1> { browse [selection_get -first] -focus}
   bind .find.list.lb <Return> {browse [selection_get -first] -focus}

   # next/prev buttons.  Get current selection index and incr/decr.
   bind .find <Down> {browseitem next .find.list.lb}
   bind .find <Up> {browseitem previous .find.list.lb}
   bind .find <Control-a> {allitems .find.list.lb}
   
   ######################################################################
 #  frame .find.search
   button .find.search -text "Search" -command searchbib
   pack .find.search  -side left -expand 1 -fill x
  # pack .find.search -expand 1 -fill x

   # message box
   message .find.m -justify left  -width 5c
   pack .find.m -side left -fill x -expand 1
   
   # the quit button
   button .find.help -text "Help" -command {briefhelp Find}
   button .find.dismiss -text "Dismiss" -command {destroy .find}
   pack .find.dismiss .find.help -side right -anchor e -fill x
}


# whenever find_field changes, relabel the menu button
proc find_fieldchange_proc {name el op} {
   global find_field
   
   .find.field.mb config -text $find_field
}

# whenever find_reftype changes, relabel the menu button
proc find_reftypechange_proc {name el op} {
   global find_reftype
   
   .find.reftype.mb config -text $find_reftype
}


#----------------------- MAIN WINDOW -------------------------------------

#---------- File menu -------------------

# save to a new file 
proc saveasbib {} {
   global fname_path  
   set fname_path [tk_getSaveFile -defaultextension .bib]
   if {$fname_path == ""} return
   testoutput 1 "saveasbib to $fname_path"
   savebib
}

# save selected entries
proc save_selected {} {
   global fname
   set selected_records [selection_get]
   if {$selected_records == ""} {return}
   set old_fname $fname
   set fname [tk_getSaveFile -defaultextension .bib]
   testoutput 1 "save <$selected_records> to $fname"
   savebib $selected_records
   set fname $old_fname
   wm title . [file tail $fname]
}

# save bibliografy, if args != "": save only these selected records
proc savebib {args} {
   global fname_path bibchanged citelist backup_max
   
   testoutput 1 "savebib to $fname_path"
   
   if {($fname_path == "") || ($fname_path == "<noname>")} {
      saveasbib
      return ;# otherwise we would have an infinite recursion
   }
   
   puts "Saving to $fname_path"
   if {[file exists $fname_path] && $backup_max != 0} {
      # we don't want to overwrite an existing file, make a backup 
      # as does bibcard to a file named bibfile.bib.~VERSION~
      # on *DOS call the backup *.bi~
      # if $backup_max > 0, make not more than $backup_max backups

      set s {}
      # find all existing backups
      catch {set s [glob $fname_path.~*]}
      
      # find the highest to date
      set max 0
      foreach i $s {
         regexp {([1-9]+)~} $i matchall vers
         if {$vers > $max} {
            set max $vers
         }
      }
      # ours is the next one in sequence
      incr max
      # if backup_max > 0, make not more than $backup_max backups
      if {$backup_max && $max > $backup_max} {
         # rotate: discard oldest backup
	 set max 1
	 while {$max < $backup_max} {
 	    set prev $max
	    incr max
	    #puts $max
	    catch {file rename -force $fname_path.~$max~ $fname_path.~$prev~}
         }
      } 	 
      # puts $max
      # rename the bibliography file to the backup name
      # give a second try if we are an a 8.3 file system (DOS)
      if [catch {file rename -force $fname_path $fname_path.~$max~} info] {
      testoutput 3 "catch returns <$info>"
      file rename -force $fname_path "[file rootname $fname_path].bi~"
      }
   }
   # write the new data
   if {$args != ""} {
      eval writebib $args $fname_path; # eval: otherwise $args is just one word
   } else {
      writebib $citelist $fname_path
   }
   set bibchanged 0
}

# fetch the selected items: if args == -first -> just the first selection
proc selection_get {args} {

   set owner [selection own]
   if {($owner != ".refs.list") && ($owner != ".find.list.lb")} {
	tk_dialog .d {Error} "Selection must be in main reference or find list window" {} -1 OK
   	return "";
   }
   set stat [catch {selection get} str]
   if {$stat > 0} {
      tk_dialog .d {No selection} {No bibliography item was selected} {} 0 OK
      set str ""
   }
      # test, whether we want only one item
   if { [lindex $args 0]  == "-first"} {
      set str [lindex $str 0]
   }    
   return $str
}

proc quit {} {
   check_save
   
   exit
}

proc check_save {} {
   global bibchanged fname debug curkey
   
   if {$bibchanged > 0} {
      set r [tk_dialog .save {Bibliography modified} \
      "Bibliography $fname has been modified since last time it was saved.  
      Do you want to save it before exiting the application?" \
      warning 0 {Save File} {Discard Changes}]
      if {$debug} {
         puts "check_save returns $r"
      }
      switch $r {
         0	{savebib}
         1	{return}
      }
   }
}


proc openbib {} {
   global fname
   
   # first close the current bib
   closebib
   
   set types {
      {{Bib Files}       {.bib}        }
      {{BibTk Files}     {.bib.tk}     }
      {{All Files}        *            }
   }
   set fname [tk_getOpenFile -filetypes $types]
   
   if {$fname != ""} {
      # Open the file ...
      bibparse $fname
  
      wm title . [file tail $fname]
   }
}

proc mergebib {} {
   global fname
   
   set types {
      {{Bib Files}       {.bib}        }
      {{All Files}        *             }
   }
   set fname [tk_getOpenFile -filetypes $types]
   
   if {$fname != ""} {
      # Open the file ...
      bibparse $fname
   }
}

proc closebib {} {
   
   global REFLIST citelist fname bibchanged
   
   # if the current bib has changed, ask whether write it to disk
   check_save
   
   # clear the internal database
   if {[info exists REFLIST]} {
      unset REFLIST
   }
   set citelist {}
   .refs.list delete 0 end
   set fname "<noname>"
   set bibchanged 0
   
   wm title . " "
   
   #  close the browse box
   if {[winfo exists .browse]} {
      destroy .browse
   }
   
}

#---------- Edit menu -------------------

#GM, 30.1. a new function edit_item with subfunctions for 
# new, duplicate, delete, rename, crossref
proc edit_item {what} {
   global citelist REFLIST bibchanged reftype_default delete_confirm
   
   if {$what != "delete"} {
      if {$what != "new"} {
	 # not new or delete, is duplicate, rename, crossref
         # get the citekey for current entry
         set oldcitekey [selection_get -first]
         if {$oldcitekey == ""} {return}
      }
      # prompt for new cite key
      set newcitekey [getnewcitekey]
      if {$newcitekey == ""} {return}
   } else {
      # for deletion: get the citekey for all selected entries
      set oldcitekey [selection_get]
      if {$delete_confirm} {
         if [tk_dialog .d {Warning} "Delete:\n$oldcitekey?" {} 0 "Yes" "No"] {
            return
         }
      }
      destroy .browse      
   }
   
   if {$what == "new"} {
      # set to default reftype
      set REFLIST($newcitekey,reftype) $reftype_default
   }

   if {$what == "crossref"} {
         set REFLIST($newcitekey,reftype) $REFLIST($oldcitekey,reftype)
         set REFLIST($newcitekey,crossref) $oldcitekey
   }
   
   if {$what == "duplicate" || $what == "rename"} {
      # copy fields from old to new
      testoutput 1 "copying $oldcitekey to $newcitekey"
      foreach el [array names REFLIST $oldcitekey,*] {
         regexp {,([a-z]+)} $el matchall fieldname
         testoutput 2 "<$fieldname> = <$REFLIST($oldcitekey,$fieldname)>"
         set REFLIST($newcitekey,$fieldname) $REFLIST($oldcitekey,$fieldname)
      }
   }
   
   if {$what == "rename" || $what == "delete"} {
      foreach ditem $oldcitekey {
         testoutput 1 "deleting item $ditem"
         # unset the old array elements
         foreach el [array names REFLIST $oldcitekey,*] {
            regexp {,([a-z]+)} $el matchall fieldname
            unset REFLIST($oldcitekey,$fieldname)
         }
         # now take them out of the citelist 
         set i [lsearch -exact $citelist $ditem]
         set citelist [lreplace $citelist $i $i]
         # update the listbox in the main window
         update_citelistbox
      }
   }
   # note that we've updated the bib
   incr bibchanged

   if {$what != "delete"} {
      append_to_citelist $newcitekey
      .refs.list selection clear 0 end
      .refs.list selection set end
      browse $newcitekey -focus
   }
} 
# end of edit_item {what}

# push the currently selected bib entry to LyX
proc pushitem {} {
        global REFLIST citelist bibchanged cite_grouping

        # get the citekey for current entry
        set items [selection_get]
        if {$items == ""} {
                tk_dialog .d {No selection} {No bibliography item was selected} {} -1 OK
                return
        }

	if {$cite_grouping} {
		LyX_insertcite [join $items ,]
	} else {
		foreach item $items {
			LyX_insertcite $item
		}
	}
}

# navigate, show (which = first|previous|next|last) item in browse box
proc browseitem {which widget} {
   set curline [$widget curselection]
   set lastline [expr [$widget index end] -1]
   # clear all selections (may be more than one)
   $widget selection clear 0 end
   # deal with 0 or multiple
   if {[llength $curline] == 0} {set curline 0}
   if {[llength $curline] > 1} {set curline [lindex $curline 0]}
   switch $which {
      first {set curline 0}
      previous {if {$curline > 0} {incr curline -1}}
      next {if {$curline < $lastline} {incr curline}}
      last {set curline $lastline}
   }
   $widget selection set $curline
   $widget see $curline
   browse [selection_get]
}

# select all items
proc allitems {widget} {
   $widget selection set 0 end
}

# bring up the find/search window which then does its own thing
proc finditem {} {
   # create search window if need be
   if {[winfo exists .find] == 0} {
      create_find
   }
   focus .find.str.e
}

# selection handler for PRIMARY requests
#
#  assume the string is in writebib_str, return the amount requested in
# the selection request.
proc selection_handler {offs maxb} {
   global	writebib_str debug
   
   if {$debug} {
      puts "selection_hander invoked: $offs $maxb"
   }
   set s [string range $writebib_str $offs [expr $offs+$maxb-1]]
   #puts "s: $s"
   return $s
}

proc selection_lost {} {
   #puts "selection lost"
}


proc copyitem_to_paste {} {
   # format the item into a string
   set cl [selection_get]
   writebib2str $cl
   # put the string to the PRIMARY selection 
   #   grab the PRIMARY selection
   selection own -command selection_lost -selection PRIMARY .
   #   create a handler for PRIMARY selection requests
   selection handle -selection PRIMARY -type STRING . selection_handler
}

# paste the current paste buffer into the bib
proc pasteitem {} {
	global bibchanged

        # get the paste buffer
        set s [selection get -selection PRIMARY]

        # parse it
        if {[catch {set dup [bibparse_s $s]}] != 0} {
                tk_dialog .d {Error} "Syntax error with pasted data\nPerhaps not BibTeX data?\nIgnoring it." {} -1 OK
	} else {
		# put it in the bib
		set REFLIST_update 0
		update_citelistbox
		puts "$dup duplicates loaded"

		incr bibchanged
	}
}

proc not_implemented {func} {
   tk_dialog .d {Unimplemented} "Feature $func is not yet implmented" {} -1 OK
}

# prompt for a new cite key
proc getnewcitekey {} {
   global citelist newcitekey
   set answer 1;  # tkdialog returns 1 for "Retry"

   # popup a box and get the new cite key
   toplevel .newcitekey
   label .newcitekey.l -text "New cite key:"
   entry .newcitekey.e 
   pack .newcitekey.l .newcitekey.e -side left
   
   # grab focus
   focus .newcitekey.e
   
   # wait for <Return> at end of line
   bind .newcitekey <Return> {set newcitekey [.newcitekey.e get]}
   
   while {$answer } {
      tkwait variable newcitekey
      # check if it exists, if so: browse it and give a warning
      if {[lsearch $citelist $newcitekey] < 0} {
         break
      } else {   
         testoutput 1 "citekey exists"
         set answer [tk_dialog .d {Warning} \
           "cite key already exists" {} 1 "Abort" "Retry" "Browse"]
         testoutput 3 "tkdialog returns $answer"
         if {$answer == 0} {set newcitekey ""}
         if {$answer == 2} {browse $newcitekey; set newcitekey ""; break}
      }
   }
   # clean up
   destroy .newcitekey
   
   return $newcitekey
}

# put into a message window, so we can compare two entries or
# copy and paste stuff to a different entry, in the long run the possibility 
# to have two browse windows might be better
proc displayitem {} {

   global REFLIST citelist
   
   # format entry for display
   set citekey [selection_get]
   append itemstring  "$REFLIST($citekey,reftype): $citekey\n"
   foreach i [array names REFLIST $citekey,*] {
      if {$REFLIST($i) != ""} {
         set j [string first "," $i]
         incr j
         append itemstring "[string range $i $j end] = $REFLIST($i)\n"
      }
   }
   # set up a window
   toplevel .displayitem
         frame .displayitem.f
      text .displayitem.f.t -width 80 -height 30 -wrap word -setgrid true\
        -yscrollcommand ".displayitem.f.scroll set" 
      scrollbar .displayitem.f.scroll -command ".displayitem.f.t yview"
      button .displayitem.b -text "OK" -command "destroy .displayitem"
      pack .displayitem.f.t -side left -fill x -expand 1
      pack .displayitem.f.scroll -side right -fill y -expand 1
      pack .displayitem.f
      pack .displayitem.b -side bottom  -anchor s 
      
      bind .displayitem q {destroy .displayitem}
      bind .displayitem <Return> {destroy .displayitem}
   wm title .displayitem "Show item"
   # insert itemtext
      
   .displayitem.f.t configure -state normal
   .displayitem.f.t delete 0.0 end 
   .displayitem.f.t insert end $itemstring
   .displayitem.f.t configure -state disabled

}

# for diagnostics, list current fields to stdout (bound to key 's')
proc showitem {} {
   global REFLIST citelist
   
   puts  ""
   set oldcitekey [selection_get]
   foreach i [array names REFLIST $oldcitekey,*] {
      if {$REFLIST($i) != ""} {
         puts stdout "($i) = $REFLIST($i)"
      }
   }
}

proc sortitems {} {
   global	citelist
   
   set citelist [lsort $citelist]
   
   update_citelistbox
}

# 18/5/2000 John Fulton <John.Fulton@iname.com>
proc printitem {} {
# memory of last print command entered
	global global_print_cmd


        # local print command
        set printcmd ""

        # popup a box and get a different print command
        toplevel .printcmd
        label .printcmd.l -text "Print command:"
        entry .printcmd.e -textvariable global_print_cmd
	
	button .printcmd.print -text "Print" -command {
		# get the item to print
		set s [selection get -selection PRIMARY]
		puts $s

		# print the entry
		writebib $s $global_print_cmd

		# clean up
		destroy .printcmd
	}
	button .printcmd.cancel -text "Cancel" -command {
		# clean up
		destroy .printcmd
	}
        pack .printcmd.l  -side left
        pack .printcmd.e -side left
        pack .printcmd.print  -ipadx 2 -side left
        pack .printcmd.cancel  -side left

        # grab focus
        set oldFocus [focus]
        focus .printcmd.e

        # wait for <Return> at end of line
        tkwait window .printcmd
        focus $oldFocus
}


#---------- Help menu -------------------
proc abouttkbibtex {} {
   global release
   toplevel .about
   
   set aboutstr "tkbibtex
   
   A portable browser/editor for BibTeX format bibliographies.
   
   by Peter Corke 1/97              (pic@cat.csiro.au)
   major modifications by G. Milde  (g.milde@physik.tu-dresden.de)
   Release $release"
   message .about.m  -width 40c -text $aboutstr
   
   button .about.www -text "Goto home page" -command {
      exec netscape -remote openURL(http://www.cat.csiro.au/dmt/programs/autom/pic/tkbibtex.html)
   }
   
   button .about.b -text "OK" -command "destroy .about"
   pack .about.m .about.www .about.b
   wm title .about "About tkbibtex"
}

proc briefhelp {topic} {
   global usage
   
   set helptopics {Usage Keys Menus Browse Find}
   
   set helpstr(Usage) $usage
   
   set helpstr(Keys) \
"Key bindings in main window:

 Selection:
   <Mouseclick>        select entry
   <Shift-Mouseclick>  select a block of entries
   <Crtl-Mouseclick>   select multiple entries
   <Ctrl-A>            Select all Items
   
  On the selected entry: 
   <Enter>             Browse entry (show in browse window for editing)
   <DoubleMouseclick>  Browse entry
   <Up>                Browse next entry
   <Down>              Browse previous entry
   
   u                   Duplicate entry           (Edit|Duplicate-entry)
   s                   Show entry (no editing)
   c                   Push citation to LyX
   <Del>               Delete entry              (Edit|Delete)
   ^C                  Copy item to paste buffer (Edit|Copy)
   ^V                  Paste from buffer         (Edit|Paste)
   
  Misc.:
   f                   Find entry                (Edit|Find)
   n                   Create new bib entry      (Edit|New-entry)
   q                   Quit
   
Keybindings in Browse window:

   <Tab> or  <Down>    Next field
   <Enter>             Next field (in one-line fields)
   <Shift Tab> or <Up> Previous field
   <Alt> + Underlined  invoces Menus and Buttons"
   
   
   set helpstr(Menus) "Tkbibtex Menus
   
File menu:
   Open           Open a file via browser window
   Merge          Merge a file via browser window (records with same
                  Citekey will overwrite those already loaded)
   SaveAs...      Save data to a different file 
   Save           Save data
   Save Selected  Save selected entries
   Close          Close the file (save if needed)
   Exit           Exit program (save if needed)
   
   for Close and Exit a prompt box will be issued if the bibliography
   has been modified but not yet saved.  When the file is written the
   previous version is saved with a unique appended number.
   
Edit menu:
   New entry        Create a new BibTeX entry
   Duplicate entry  Duplicate the current selection but assign new citekey
   Crossref entry   Create a new citation with cross reference to the current selection
   Find entry       Invoke the search window
   Delete entry     Delete the selected entries from the database
   Copy to buffer   Copy selected entries to PRIMARY cut buffer, can be
                    pasted into other applications.
   Insert to LyX    Push citation to Lyx
   Show entry       Show entry in a message window (no editing)
   Sort             Sort keys alphabetically
   Print            Print bib items"
   
   set helpstr(Find) "Search/find window:

   * Find all entries containing the searchstring in specified field
   * Seachstring is a regular expression: .*?()[]\ have special meanings
   * Either start year or end year or both may be specified.
   * The entries found are displayed in a list which can be browsed by the
     same key bindings as in the main window:

     Selection:
      <Mouseclick>         select entry
      <Shift-Mouseclick>   select a block of entries
      <Crtl-Mouseclick>    select multiple entries
      <Ctrl-A>             Select all Items
      
     On the selected entry: 
      <Enter>              Browse entry (show in browse window for editing)
      <Double-Mouseclick>  Browse entry
      <Up>                 Browse next entry
      <Down>               Browse previous entry"
     
     
   set helpstr(Browse) "Browse box:

* The browse box shows all fields for the specified entry, 
  allowing for editing

      !  BibTeX distinguishes between 
         - normal fieldtext (enclosed in brackets or double quotes), 
         - numbers (may be enclosed or not) and 
         - strings (words beginning with a character, not enclosed).
         
      -> Normal text entries will become enclosed in brackets.
      -> If an entry shall be written to the file as is, precede it with 
         a hash (e.g. month strings -> #jan - #dec).
         
* The reftype button brings up a menu of reference types to choose from

* Black lines seperate the three different types of field BibTeX knows
  about: 
     Required fields   (have to be present to get a well formed citation)
     Optional fields   (not necessary but show up in the citation if present)
     Additional fields (ignored by the standard BibTeX styles
                        but may contain usefull information)               

* Some fields are foldable multiple line entries that unfold if the label
  button is clicked on.
  
* Right clicking in an entry box brings up either:
	* a menu of standard monthnames (if a month entry)
	* jump to the citation that is cross-referenced (if a crossref entry)
	* a menu of string items (if there are any defined) which will 
	  be inserted.

* Keybindings:
   <Tab> or  <Down>     Next field
   <Enter>              Next field (in one-line fields)
   <Shift Tab> or <Up>  Previous field
   <Alt> + Underlined   invoces Buttons"
   
   
   # set up the help window
   # (show help in a text widget, so we can incorporate hypertext tags later)
   if {![winfo exists .briefhelp]} {
      toplevel .briefhelp
      
      frame .briefhelp.m 
      label .briefhelp.m.l -text "Topics:"
      pack .briefhelp.m 
      pack .briefhelp.m.l -side left
      foreach top $helptopics {
         button .briefhelp.m.[string tolower $top] -text $top -underline 0 \
           -command "briefhelp $top"
         pack .briefhelp.m.[string tolower $top] -side left
         
         bind .briefhelp <Alt-Key-[string index [string tolower $top] 0]> \
           "briefhelp $top; break"
      }
      #.briefhelp configure -menu .briefhelp.m does not work as desired
      
      
      frame .briefhelp.f
      text .briefhelp.f.t -width 80 -height 20 \
        -yscrollcommand ".briefhelp.f.scroll set" 
      scrollbar .briefhelp.f.scroll -command ".briefhelp.f.t yview"
      button .briefhelp.b -text "OK" -command "destroy .briefhelp"
      pack .briefhelp.f.t -side left -fill x -expand 1
      pack .briefhelp.f.scroll -side right -fill y -expand 1
      pack .briefhelp.f
      pack .briefhelp.b -side bottom  -anchor s 
      
      bind .briefhelp q {destroy .briefhelp}
      bind .briefhelp <Escape> {destroy .briefhelp}
      wm title .briefhelp "Tkbibtex Help"
   } else {
      foreach top $helptopics {
         .briefhelp.m.[string tolower $top] configure -state normal
      }
   }
   # make the topics button active
   .briefhelp.m.[string tolower $topic] configure -state active 

   # insert the topic
   
   .briefhelp.f.t configure -state normal
   .briefhelp.f.t delete 0.0 end 
   .briefhelp.f.t insert end $helpstr($topic)
   .briefhelp.f.t configure -state disabled

}

# handle changes to iconify/deiconify state, make all windows adopt the same
# state.
proc wm_statechange {w} {
	global	window_state

	# check if window state has changed
	set toplevel [winfo toplevel $w]
	if {[wm state $toplevel] != $window_state} {
		#puts "window state changed $window_state"
		set window_state [wm state .]
		if {[wm state $toplevel] == "normal"} {
			# unminimize all windows
			wm deiconify .
			if {[winfo exists .find]} {
				wm deiconify .find
			}
			if {[winfo exists .browse]} {
				wm deiconify .browse
			}
		} elseif {[wm state $toplevel] == "iconic"} {
			# minimize all windows
			wm iconify .
			if {[winfo exists .find]} {
				wm iconify .find
			}
			if {[winfo exists .browse]} {
				wm iconify .browse
			}
		}
	}
}
#---------- create the main window ------------------
proc create_main {} {
   global alltypes refs_height refs_width
   
   
#   # create the menu bar
   menu .mbar -tearoff 0
   .mbar add cascade -label "File" -menu .mbar.file -underline 0
   .mbar add cascade -label "Edit" -menu .mbar.edit -underline 0
   .mbar add cascade -label "Help" -menu .mbar.help -underline 0

# declare the menu to the main window
   . configure -menu .mbar

   # create the File menu
   menu .mbar.file
#   .mbar.file add command -label "New" -underline 0 -command closebib
#   this might beocme usefull if we can open multiple files...
   .mbar.file add command -label "Open" -underline 0 \
        -accelerator F3 -command openbib
   .mbar.file add command -label "Merge" -underline 0 -command mergebib
   .mbar.file add command -label "SaveAs" -underline 4 \
    	-accelerator Shift-F2 -command saveasbib
   .mbar.file add command -label "Save" -underline 0 \
    	-accelerator "^S / F2" -command savebib
   .mbar.file add command -label "Save Selected" -underline 7 \
      -command {save_selected}
   .mbar.file add command -label "Print Selected ..." -underline 0 \
       -accelerator P -command printitem
   .mbar.file add command -label "Close" -underline 0 \
    	-accelerator Shift-F3 -command closebib
   .mbar.file add command -label "Exit" -underline 0 \
    	-accelerator ^Q -command quit
   
   # create the edit menu
   menu .mbar.edit
   .mbar.edit add command -label "New entry" -underline 0 \
       -accelerator N -command {edit_item new}
   .mbar.edit add command -label "Duplicate entry" -underline 1 \
       -accelerator U -command {edit_item duplicate}
   .mbar.edit add command -label "Cross-reference entry" \
       -accelerator X -command {edit_item crossref}
   .mbar.edit add command -label "Find entry ..." -underline 0 \
       -accelerator F -command finditem
   .mbar.edit add command -label "Rename entry" -underline 0 \
       -accelerator R -command {edit_item rename}
   .mbar.edit add command -label "Delete entry" -underline 0 \
       -accelerator <Del> -command {edit_item delete}
   .mbar.edit add command -label "Push to LyX" -underline 0 \
       -accelerator c -command pushitem
   .mbar.edit add command -label "Copy to buffer" -underline 0 \
       -accelerator ^C -command copyitem_to_paste
   .mbar.edit add command -label "Paste buffer" -underline 0 \
       -accelerator ^V -command pasteitem
   # for diagnostics, list current fields 
   .mbar.edit add command -label "Show entry" -underline 0 \
       -accelerator S -command displayitem
   .mbar.edit add command -label "Sort" -underline 1 -command sortitems
   
   # create the help menu
   menu .mbar.help
   .mbar.help add command -label "Brief help" -underline 0 \
       -accelerator <F1> -command {briefhelp Keys}
   .mbar.help add command -label "About tkbibtex" -underline 0 \
       -command abouttkbibtex
   
   menubutton .new -relief raised -text "New" -menu .new.menu
   menu .mbar.edit.new
   foreach i $alltypes {
      .mbar.edit.new add command -label $i  -command "new_item $i"
   }
   
   # now the message window
   message .m -relief groove -width 30c -justify  left -anchor w
   pack .m -fill x -expand 1
   
   # then the scrolling list of citations
   frame .refs
   listbox .refs.list -height $refs_height -width $refs_width\
      -yscrollcommand ".refs.scroll set" -selectmode extended
   scrollbar .refs.scroll -command ".refs.list yview"
   
   pack .refs.list -side left -fill x -fill both -expand true 
   pack .refs.scroll -side right -fill y 
   pack .refs -side bottom -expand true -fill both
   
   # double click on the cite key to display it in browse box
   bind .refs.list <Double-Button-1> {browse [selection_get -first] -focus}
   bind . <Return> {browse [selection_get -first] -focus}
   bind . <Down> {browseitem next .refs.list}
   bind . <Up> {browseitem previous .refs.list}
   
   # some helpful keyboard bindings
   bind Button <Key-Return> "tkButtonInvoke %W"; # Enter invokes Buttons
   bind . c {pushitem}
   bind . u {edit_item duplicate}
   bind . f {finditem}
   bind . <Alt-f> { }; # dummy entry to enable Menu shortcut
   bind . n {edit_item new}
   bind . p {printitem}
   bind . <Control-q> {quit}
   bind . q {quit}
   bind . r {edit_item rename}
   bind . s {displayitem}
   bind . <Control-c> copyitem_to_paste
#   bind . <Control-Key-Insert> copyitem_to_paste
# this binding does prevent copyitem_to_paste from working! (Why?)
   bind . <Control-v> pasteitem
#   bind . <Shift-Key-Insert> pasteitem
# this binding does prevent pasteitem from working! (Why?)
   bind . <Control-a> {allitems .refs.list}
   bind . <Delete> {edit_item delete}
   bind . <Unmap> {wm_statechange %W}
   bind . <Map> {wm_statechange %W}
   wm protocol . WM_DELETE_WINDOW quit
   wm protocol . WM_SAVE_YOURSELF quit
}

# show one line status messages in the main window
proc showmsg {m} {
   global interactive_mode

   if {$interactive_mode} {
	   .m config -text $m
	   update idletasks
   }
}

#---------------------- LyX support -----------------------

# Interface to Lyx based on development/LyXserver/sampleclient.tcl
# =========================================================================
#   File: sampleclient.tcl, chb, Sun 05.11.1995 (19:24)
#   sampleclient.tcl,v 1.1.1.1 1996/08/19 14:39:38 larsbj Exp
#   This file contains examples for communicating to LyX via the
#   LyXserver interface. It contains a panel of shortcuts to
#   demonstrate how users can define their own macros. It also shows
#   how new commands can be built into LyX using the 'notify' mechanism.
# =========================================================================


# --- 1. check cmdargs (pipename) -----------------------------------------

set LyXChan -1

proc LyX_connect {} {
   global LyXpipe LyXChan
   
   set inpipe  $LyXpipe.in
   
   if { [file exists $inpipe] } {
      if { "[file type $inpipe]" != "fifo" }  {
	 tk_dialog .d {LyX connection} "LyXpipe <$inpipe> is not a pipe!" {} -1 OK
      }
      puts "Connecting to LyX via $LyXpipe.in"
      set LyXChan [open $inpipe {WRONLY NONBLOCK}]
puts "<LyxChan $LyXChan>"
   } else {
	 tk_dialog .d {LyX connection} "No LyXpipe ($LyXpipe.*) exists\nCheck your .LyXrc file" {} -1 OK
   }
   
}

# ---
#  Format for communication:
#
#    client->LyX:          "LYXCMD:<client>:<command>:<argument>" >$inpipe
#    LyX->client (answer): "INFO:<client>:<command>:<data>"       <$outpipe
#    LyX->client (notify): "NOTIFY:<key-sequence>"                <$outpipe
#    (notifies are asynchroneous and are handled by 'outputhandler'
#     above)
# ---

proc LyX_insertcite {cite} {
   global LyXChan
   
   # open the pipe each time, in case LyX crashes...
   LyX_connect
   puts "sending citation $cite to LyX"
   puts $LyXChan "LYXCMD:sampleclient:citation-insert:$cite"
   close $LyXChan
}

#---------------------- Auxilliary functions -----------------------------

# Put a text on stdout, if the global var debug is >= debug_level
proc testoutput {debug_level text} {
   global debug
   if {$debug >= $debug_level} {puts stdout $text}
} 

proc show_usage {} {
   global	usage
   puts stdout $usage
   exit
}


#------------------------------ MAIN BODY ----------------------------

#warn about wrong version
if {[info tclversion] < 8} {
   puts stdout "tkbibtex needs Tcl/Tk version 8.0 or later"
}

# handle command line switches
while {[llength $argv] > 0} {
   if {$debug} {
      puts [lindex $argv 0]
   }
   switch -regexp -- [lindex $argv 0] {
      ^-debug     {	set debug [lindex $argv 1]; \
           set argv [lrange $argv 2 end] ; \
        }
      ^-ys     {	set arg_ys [lindex $argv 1]; \
           set argv [lrange $argv 2 end]; \
           incr command_line_mode
      }
      ^-ye     {	set arg_ye [lindex $argv 1]; \
           set argv [lrange $argv 2 end]; \
           incr command_line_mode
      }
      ^-last   {	set t [clock seconds]; \
           set arg_ye [clock format $t -format %Y]; \
           set arg_ys [expr $arg_ye-[lindex $argv 1]]; \
           set argv [lrange $argv 2 end]; \
           incr command_line_mode
      }
      ^-type   {	set arg_reftype [lindex $argv 1]; \
           set argv [lrange $argv 2 end]; \
           incr command_line_mode
      }
      ^-field  {	set arg_field [lindex $argv 1]; \
           set argv [lrange $argv 2 end]; \
           incr command_line_mode
      }
      ^-search  {	set arg_search [lindex $argv 1]; \
           set argv [lrange $argv 2 end]; \
           incr command_line_mode
      }
      ^-exact	-
      ^-case	{	set arg_ignorecase 0; \
           set argv [lrange $argv 1 end]; \
           incr command_line_mode
      }
      ^-count	{	set arg_count 1; \
           set argv [lrange $argv 1 end]; \
           incr command_line_mode
      }
      ^-l      -
      ^-list   {       set arg_list 1; \
                      set argv [lrange $argv 1 end]; \
                      incr command_line_mode
      }
      ^-k     -
      ^-key   {       set arg_key 1; \
                      set argv [lrange $argv 1 end]; \
                      incr command_line_mode
      }
      ^-select  {	set arg_select [lindex $argv 1]; \
           set argv [lrange $argv 2 end]; \
           incr command_line_mode
      }
      ^-lyx   {	set LyXpipe [lindex $argv 1]; \
                set argv [lrange $argv 2 end]; \
        }
      ^-help	-
      ^-.*	{	show_usage}
      default	{break}
   }
}


if {$command_line_mode} {
   # COMMAND LINE MODE, NO DISPLAY
   set interactive_mode false
   
   # read string file if it exists
   set strfname [find_on_path $strfile]
   if {$strfname != {}} {
      bibparse_f $strfname
   }

# load the specified bibtex files if they exist
   foreach f $argv {
      set fp [find_on_path $f]
      if {$fp != {}} {
         bibparse_f $fp
      }
   }
   
   set found [do_search $arg_search $arg_reftype $arg_field $arg_ys \
     $arg_ye $arg_ignorecase]
   if {$arg_count} {
      puts stdout [llength $found]
   } elseif {$arg_key} {
	foreach k $found {
		puts stdout $k
	}
   } elseif {$arg_list} {
	foreach k $found {
		puts stdout "$k $REFLIST($k,reftype) $REFLIST($k,title)"
	}
   } elseif {$arg_select != {}} {
      puts stdout  [writebib2str $arg_select]
   } else {
      puts stdout  [writebib2str $found]
   }
   exit 0
} else {
   # INTERACTIVE MODE
   
   set interactive_mode true
   # create the main window
   create_main
   
   update idletasks
   
   # read string file if it exists
   set strfname [find_on_path $strfile]
   if {$strfname != {}} {
      bibparse $strfname
   }
   
   # load the specified bibtex files if they exist
   foreach f $argv {
      set fname $f
      set fp [find_on_path $f]
      if {$fp != {}} {
         bibparse_f $fp
      }
      set fname_path $fp
      update_citelistbox
   }
   
   # unset global filename if more than one input file
   if {[llength $argv] >  1} {
      set fname "<noname>"
   }

   wm title . $fname
  
   showmsg ""
}
