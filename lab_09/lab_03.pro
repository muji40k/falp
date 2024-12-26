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

    paternal_grand_mother(name, name).
    paternal_grand_father(name, name).
    maternal_grand_mother(name, name).
    maternal_grand_father(name, name).
    maternal_grand_parent(name, name).

    max2(integer, integer, integer).
    max3(integer, integer, integer, integer).
    max2_cut(integer, integer, integer).
    max3_cut(integer, integer, integer, integer).

clauses
    % #4
    paternal_grand_mother(Child, Grand) :- father(Child, P), mother(P, Grand).
    paternal_grand_father(Child, Grand) :- father(Child, P), father(P, Grand).
    maternal_grand_mother(Child, Grand) :- mother(Child, P), mother(P, Grand).
    maternal_grand_father(Child, Grand) :- mother(Child, P), father(P, Grand).

    % #1
    grand_mother(Child, Grand) :- maternal_grand_mother(Child, Grand).
    grand_mother(Child, Grand) :- paternal_grand_mother(Child, Grand).
    % #2
    grand_father(Child, Grand) :- maternal_grand_father(Child, Grand).
    grand_father(Child, Grand) :- paternal_grand_father(Child, Grand).
    % #3
    grand_parent(Child, GMother) :- grand_mother(Child, GMother).
    grand_parent(Child, GFather) :- grand_father(Child, GFather).

    % #5
    maternal_grand_parent(Child, GMother) :- maternal_grand_mother(Child, GMother).
    maternal_grand_parent(Child, GFather) :- maternal_grand_father(Child, GFather).

    mother("Jackson Jeffery", "Calle Lori").
    mother("Alvarez Yolanda", "Mckinney Dorothy").
    mother("Held Joseph", "Cleaves Lucinda").
    mother("Delcid Sophia", "Broadus Beverly").
    mother("Harrison Marcos", "Medlin Suzanna").
    mother("Girard Deborah", "Mcbride Irene").
    mother("Mitchell Leroy", "Bucher Florence").
    mother("Suarez Marie", "Daquilante Jill").
    mother("Tan Gerald", "Smidt Jana").
    mother("Stewart Lillian", "Morales Jennifer").
    mother("Israel Dominick", "Germain Eliza").
    mother("Smith Justine", "Gibeau Cathy").
    mother("Long John", "Vazquez Linda").
    mother("Collier Laverne", "Fair Susan").
    mother("Kingsbury Douglas", "Ibarra Imelda").
    mother("Lehman Beulah", "Longo Kathleen").
    mother("Held Abel", "Delcid Sophia").
    mother("Jackson Pauline", "Alvarez Yolanda").
    mother("Mitchell Marion", "Suarez Marie").
    mother("Harrison Lorraine", "Girard Deborah").
    mother("Tan Robert", "Stewart Lillian").
    mother("Israel Alana", "Smith Justine").
    mother("Long Ronald", "Collier Laverne").
    mother("Kingsbury Kristina", "Lehman Beulah").
    mother("Held Robert", "Jackson Pauline").
    mother("Mitchell Mary", "Harrison Lorraine").
    mother("Tan Norman", "Israel Alana").
    mother("Long Dorothy", "Kingsbury Kristina").
    mother("Held Nathaniel", "Mitchell Mary").
    mother("Tan Sabrina", "Long Dorothy").
    mother("Held Tiffany", "Tan Sabrina").

    father("Jackson Jeffery", "Jackson Fredrick").
    father("Alvarez Yolanda", "Alvarez David").
    father("Held Joseph", "Held Thomas").
    father("Delcid Sophia", "Delcid Bill").
    father("Harrison Marcos", "Harrison John").
    father("Girard Deborah", "Girard George").
    father("Mitchell Leroy", "Mitchell Oliver").
    father("Suarez Marie", "Suarez Alfred").
    father("Tan Gerald", "Tan Frank").
    father("Stewart Lillian", "Stewart George").
    father("Israel Dominick", "Israel Marc").
    father("Smith Justine", "Smith Javier").
    father("Long John", "Long Donald").
    father("Collier Laverne", "Collier David").
    father("Kingsbury Douglas", "Kingsbury Scott").
    father("Lehman Beulah", "Lehman Randy").
    father("Held Abel", "Held Joseph").
    father("Jackson Pauline", "Jackson Jeffery").
    father("Mitchell Marion", "Mitchell Leroy").
    father("Harrison Lorraine", "Harrison Marcos").
    father("Tan Robert", "Tan Gerald").
    father("Israel Alana", "Israel Dominick").
    father("Long Ronald", "Long John").
    father("Kingsbury Kristina", "Kingsbury Douglas").
    father("Held Robert", "Held Abel").
    father("Mitchell Mary", "Mitchell Marion").
    father("Tan Norman", "Tan Robert").
    father("Long Dorothy", "Long Ronald").
    father("Held Nathaniel", "Held Robert").
    father("Tan Sabrina", "Tan Norman").
    father("Held Tiffany", "Held Nathaniel").

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
    % uncle("Terry Docken", Bro).
    % brother("Mattie Bays", Bro).

