# SPDX-License-Identifier: GPL-2.0-or-later
# RingsForHomalg: Dictionaries of external rings
#
# This file contains package meta data. For additional information on
# the meaning and correct usage of these fields, please consult the
# manual of the "Example" package as well as the comments in its
# PackageInfo.g file.
#

SetPackageInfo( rec(

PackageName := "LiEForGAP",
Subtitle := "Dictionary of LiE and GAP",
Version := "2022.10-20",
Date := Concatenation( "01/", ~.Version{[ 6, 7 ]}, "/", ~.Version{[ 1 .. 4 ]} ),
License := "GPL-2.0-or-later",

Persons := [
  rec(
    FirstNames := "VInay",
    LastName := "Wagh",
    IsAuthor := true,
    IsMaintainer := true,
    Email := "waghoba@gmail.com",
    WWWHome := "https://www.iitg.ac.in/vinay.wagh/",
    PostalAddress := Concatenation( [
                       "E-102, Department of Mathematics,\n",
                       "Indian Institute of Technology Guwahati,\n",
                       "Guwahati, Assam, India.\n",
                       "PIN: 781 039.\n",
                       "India" ] ),
    Place         := "Guwahati",
    Institution   := "Indian Institute of Technology Guwahati"
  ),
],

Status := "dev",

# BEGIN URLS
SourceRepository := rec(
    Type := "git",
    URL := "https://github.com/wagh/LiEForGAP",
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome := Concatenation( ~.SourceRepository.URL, "/" ),
PackageInfoURL := Concatenation( ~.SourceRepository.URL, "/PackageInfo.g" ),
README_URL := Concatenation( ~.SourceRepository.URL, "/README.md" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL, "-", ~.Version ),
# END URLS

ArchiveFormats := ".tar.gz .zip",

AbstractHTML := 
"The <span class=\"pkgname\">LiEForGAP</span> package provides small dictionary for \
 <span class=\"pkgname\">GAP</span>",
PackageDoc := rec(
  BookName  := "LiEForGAP",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Dictionary of LiE and GAP",
),


Dependencies := rec(
  GAP := ">= 4.12",
  NeededOtherPackages := [ ],
  SuggestedOtherPackages := [ ],
  ExternalConditions := [ ]
),

AvailabilityTest := function()
    return true;
  end,

TestFile := "tst/testall.g",

Keywords := [ "LiE algebra", "Tensor Product Decomposition", "Weyl diagrams" ],

AutoDoc := rec(
    TitlePage := rec(
        Copyright := Concatenation(
            "&copyright; 2022-2022 by VInay Wagh\n\n",
            "This package may be distributed under the terms and conditions ",
            "of the GNU Public License Version 2 or (at your option) any later version.\n"
            )
    )
),

));
