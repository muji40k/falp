/*************************************************************************

        Copyright (c) My Company

 Project:  LAB_03
 FileName: LAB_03.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
**************************************************************************/

domains
    name = string.

predicates
    mother(name, name).
    father(name, name).

    grand_parent(name, name).
    grand_mother(name, name).
    grand_father(name, name).

% =====================================================

    mother_n(name, integer, name).

    %1
    family(name, name).
    %2
    same_parents(name, name).
    brother(name, name).
    sister(name, name).
    %3
    hmother(name, name).
    hfather(name, name).
    wmother(name, name).
    wfather(name, name).
    %4
    uncle(name, name).
    aunt(name, name).
    %5
    step_brother(name, name).
    step_sister(name, name).

    female(name).
    male(name).

% =====================================================

    paternal_grand_mother(name, name).
    paternal_grand_father(name, name).
    maternal_grand_mother(name, name).
    maternal_grand_father(name, name).

    max2(integer, integer, integer).
    max3(integer, integer, integer, integer).
    max2_cut(integer, integer, integer).
    max3_cut(integer, integer, integer, integer).

clauses
    paternal_grand_mother(Child, Grand) :- father(Child, P), mother(P, Grand).
    paternal_grand_father(Child, Grand) :- father(Child, P), father(P, Grand).
    maternal_grand_mother(Child, Grand) :- mother(Child, P), mother(P, Grand).
    maternal_grand_father(Child, Grand) :- mother(Child, P), father(P, Grand).

    grand_mother(Child, Grand) :- maternal_grand_mother(Child, Grand).
    grand_mother(Child, Grand) :- paternal_grand_mother(Child, Grand).
    grand_father(Child, Grand) :- maternal_grand_father(Child, Grand).
    grand_father(Child, Grand) :- paternal_grand_father(Child, Grand).

    grand_parent(Child, GMother) :- grand_mother(Child, GMother).
    grand_parent(Child, GFather) :- grand_father(Child, GFather).

    maternal_grand_parent(Child, GMother) :- maternal_grand_mother(Child, GMother).
    maternal_grand_parent(Child, GFather) :- maternal_grand_father(Child, GFather).

% =====================================================

    mother_n(_, Depth, _) :- 1 > Depth, fail.
    mother_n(Child, 1, GMother) :- mother(Child, GMother), !.
    mother_n(Child, Depth, GMother) :- Next = Depth - 1,
                                       mother(Child, Mother),
                                       mother_n(Mother, Next, GMother).
    mother_n(Child, Depth, GMother) :- Next = Depth - 1,
                                       father(Child, Father),
                                       mother_n(Father, Next, GMother).

    family(Wife, Husband) :- mother(C, Wife), father(C, Husband), !.
    same_parents(P1, P2) :- not(P1 = P2), father(P1, M), father(P2, M), !.
    same_parents(P1, P2) :- not(P1 = P2), mother(P1, M), mother(P2, M).
    brother(Person, Brother) :- male(Brother), same_parents(Person, Brother).
    sister(Person, Sister) :- female(Sister), same_parents(Person, Sister).

    hmother(Wife, Mother) :- family(Wife, Husband), mother(Husband, Mother).
    hfather(Wife, Father) :- family(Wife, Husband), father(Husband, Father).
    wmother(Husband, Mother) :- family(Wife, Husband), mother(Wife, Mother).
    wfather(Husband, Father) :- family(Wife, Husband), father(Wife, Father).

    uncle(Child, Uncle) :- father(Child, Father), brother(Father, Uncle).
    uncle(Child, Uncle) :- mother(Child, Mother), brother(Mother, Uncle).
    aunt(Child, Aunt) :- father(Child, Father), sister(Father, Aunt).
    aunt(Child, Aunt) :- mother(Child, Mother), sister(Mother, Aunt).

    step_brother(Child, SBrother) :- uncle(Child, Uncle),
                                     father(SBrother, Uncle),
                                     male(SBrother).
    step_brother(Child, SBrother) :- aunt(Child, Aunt),
                                     mother(SBrother, Aunt),
                                     male(SBrother).
    step_sister(Child, SSister) :- uncle(Child, Uncle),
                                   father(SSister, Uncle),
                                   female(SSister).
    step_sister(Child, SSister) :- aunt(Child, Aunt),
                                   mother(SSister, Aunt),
                                   female(SSister).

% =====================================================

    female("Retta Greening").
    female("Maria Riddle").
    female("Diane Brown").
    female("Paula Chidester").
    female("Doris Henderson").
    female("Ruth Campbell").
    female("Mary Huett").
    female("Carolina Dixon").
    female("Margaret Stamper").
    female("Mattie Bays").
    female("Brittany Davis").
    female("Victoria Nicholson").
    female("Margaret Nicholson").
    female("Christina Nicholson").
    female("Chelsea Schermann").
    female("Suzanne Schermann").
    female("Irene Bays").
    female("Joan Anderson").
    female("Jean Anderson").
    female("Sonja Docken").
    female("Vannessa Dejohn").
    female("Patricia Dejohn").
    female("Linda Cecil").
    female("Teena Docken").
    female("Liz Schermann").
    female("Patricia Schermann").
    female("Opal Docken").
    female("Linda Dejohn").
    female("Diane Dejohn").
    female("Beulah Dejohn").
    female("Marjorie Bays").
    female("Sharon Bays").
    female("Nancy Bays").
    female("Wendi Docken").
    male("Ruben Stamper").
    male("David Anderson").
    male("Cornelius Bays").
    male("Donald Dejohn").
    male("Kent Cecil").
    male("Anthony Docken").
    male("Christopher Davis").
    male("Thomas Nicholson").
    male("Richard Schermann").
    male("Michael Anderson").
    male("Howard Bays").
    male("Darryl Bays").
    male("Jorge Dejohn").
    male("Kenneth Cecil").
    male("Anthony Cecil").
    male("Jacob Docken").
    male("Charles Docken").
    male("Calvin Docken").
    male("Dominick Schermann").
    male("Manuel Cecil").
    male("Donald Bays").
    male("Robert Docken").
    male("Emil Docken").
    male("Terry Docken").
    male("Elvin Dejohn").
    male("Mark Docken").
    mother("Margaret Stamper", "Retta Greening").
    mother("Michael Anderson", "Maria Riddle").
    mother("Jorge Dejohn", "Diane Brown").
    mother("Kenneth Cecil", "Paula Chidester").
    mother("Anthony Cecil", "Paula Chidester").
    mother("Jacob Docken", "Doris Henderson").
    mother("Charles Docken", "Doris Henderson").
    mother("Calvin Docken", "Doris Henderson").
    mother("Brittany Davis", "Ruth Campbell").
    mother("Victoria Nicholson", "Mary Huett").
    mother("Margaret Nicholson", "Mary Huett").
    mother("Christina Nicholson", "Mary Huett").
    mother("Chelsea Schermann", "Carolina Dixon").
    mother("Suzanne Schermann", "Carolina Dixon").
    mother("Dominick Schermann", "Carolina Dixon").
    mother("Manuel Cecil", "Brittany Davis").
    mother("Irene Bays", "Chelsea Schermann").
    mother("Joan Anderson", "Margaret Stamper").
    mother("Jean Anderson", "Margaret Stamper").
    mother("Donald Bays", "Victoria Nicholson").
    mother("Robert Docken", "Christina Nicholson").
    mother("Emil Docken", "Mattie Bays").
    mother("Sonja Docken", "Mattie Bays").
    mother("Terry Docken", "Mattie Bays").
    mother("Vannessa Dejohn", "Margaret Nicholson").
    mother("Elvin Dejohn", "Margaret Nicholson").
    mother("Patricia Dejohn", "Margaret Nicholson").
    mother("Linda Cecil", "Suzanne Schermann").
    mother("Opal Docken", "Liz Schermann").
    mother("Linda Dejohn", "Linda Cecil").
    mother("Diane Dejohn", "Linda Cecil").
    mother("Beulah Dejohn", "Linda Cecil").
    mother("Marjorie Bays", "Teena Docken").
    mother("Sharon Bays", "Teena Docken").
    mother("Nancy Bays", "Teena Docken").
    mother("Wendi Docken", "Jean Anderson").
    mother("Mark Docken", "Jean Anderson").
    father("Margaret Stamper", "Ruben Stamper").
    father("Michael Anderson", "David Anderson").
    father("Howard Bays", "Cornelius Bays").
    father("Darryl Bays", "Cornelius Bays").
    father("Mattie Bays", "Cornelius Bays").
    father("Jorge Dejohn", "Donald Dejohn").
    father("Kenneth Cecil", "Kent Cecil").
    father("Anthony Cecil", "Kent Cecil").
    father("Jacob Docken", "Anthony Docken").
    father("Charles Docken", "Anthony Docken").
    father("Calvin Docken", "Anthony Docken").
    father("Brittany Davis", "Christopher Davis").
    father("Victoria Nicholson", "Thomas Nicholson").
    father("Margaret Nicholson", "Thomas Nicholson").
    father("Christina Nicholson", "Thomas Nicholson").
    father("Chelsea Schermann", "Richard Schermann").
    father("Suzanne Schermann", "Richard Schermann").
    father("Dominick Schermann", "Richard Schermann").
    father("Manuel Cecil", "Anthony Cecil").
    father("Irene Bays", "Darryl Bays").
    father("Joan Anderson", "Michael Anderson").
    father("Jean Anderson", "Michael Anderson").
    father("Donald Bays", "Howard Bays").
    father("Robert Docken", "Charles Docken").
    father("Emil Docken", "Jacob Docken").
    father("Sonja Docken", "Jacob Docken").
    father("Terry Docken", "Jacob Docken").
    father("Vannessa Dejohn", "Jorge Dejohn").
    father("Elvin Dejohn", "Jorge Dejohn").
    father("Patricia Dejohn", "Jorge Dejohn").
    father("Linda Cecil", "Kenneth Cecil").
    father("Teena Docken", "Calvin Docken").
    father("Liz Schermann", "Dominick Schermann").
    father("Patricia Schermann", "Dominick Schermann").
    father("Opal Docken", "Robert Docken").
    father("Linda Dejohn", "Elvin Dejohn").
    father("Diane Dejohn", "Elvin Dejohn").
    father("Beulah Dejohn", "Elvin Dejohn").
    father("Marjorie Bays", "Donald Bays").
    father("Sharon Bays", "Donald Bays").
    father("Nancy Bays", "Donald Bays").
    father("Wendi Docken", "Terry Docken").
    father("Mark Docken", "Terry Docken").

    max2(A, B, Res) :- A >= B, Res = A.
    max2(A, B, Res) :- A < B, Res = B.

    max2_cut(A, B, A) :- A >= B, !.
    max2_cut(_, B, B).

    max3(A, B, C, Res) :- A >= B, A >= C, Res = A.
    max3(A, B, C, Res) :- B > A, B >= C, Res = B.
    max3(A, B, C, Res) :- C > A, C > B, Res = C.

    max3_cut(A, B, C, A) :- A >= B, A >= C, !.
    max3_cut(_, B, C, B) :- B >= C, !.
    max3_cut(_, _, C, C).

goal
    % mother_n("Held Tiffany", 3, Parent).
    % mother_n(Child, 3, "Cleaves Lucinda").
    % grand_mother("Mitchell Marion", GMother).
    % brothers(B, "Held Nathaniel").
    % hmother("Israel Alana", HMother), hfather("Israel Alana", HFather).
    % wmother("Tan Robert", WMother), wfather("Tan Robert", WFather).

    % step_sister("Terry Docken", Bro).
    % hmother("Mattie Bays", Mother).
    % family("Mattie Bays", Hus).
    uncle("Terry Docken", Bro).
% brother("Mattie Bays", Bro).

