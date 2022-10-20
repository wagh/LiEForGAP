# SPDX-License-Identifier: GPL-2.0-or-later
# RingsForHomalg: Dictionaries of external rings
#
# Reading the implementation part of the package.
#

## init
# ReadPackage( "RingsForHomalg", "gap/Oscar.gi" );

if IsBound( MakeThreadLocal ) then
    Perform(
            [
             "CommonTableForLiEBasics",
             "CommonTableForLiETools",
             ],
            MakeThreadLocal );
fi;
