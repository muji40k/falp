/*************************************************************************

        Copyright (c) My Company

 Project:  LAB_02
 FileName: LAB_02.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
**************************************************************************/

domains
    name = string.

    phone = string.
    city = string.
    street = string.

    mark = string.
    color = string.
    reg_number = string.

    house = integer.
    flat = integer.
    cost = integer.
    addr = address(city, street, house, flat).

    area = real.
    latitude = real.
    longitude = real.
    coord = coordinate(latitude, longitude).

    ownership = car(mark, color, reg_number);
                building(city, street, house);
                land_plot(coord, area);
                ship(mark, color).

    int_list = integer*.

predicates
    phone_book(name, phone, addr).
    owner(name, cost, ownership).

    sum_int_list(int_list, integer).
    in_sum_int_list(int_list, integer, integer).
    sum_costs(name, cost).

clauses
    phone_book("Karpov", "5-449-47-81",
               address("Petuhovo", "Tsentral'naja", 5, 610)).
    phone_book("Fedorova", "4-533-24-03",
               address("Gorohovets", "Molodezhnaja", 35, 48)).
    phone_book("Jashina", "1-267-52-39",
               address("Ob'", "Shkol'naja", 48, 278)).
    phone_book("Bogdanova", "3-967-14-77",
               address("Egor'evsk", "Lesnaja", 56, 857)).
    phone_book("Koroleva", "4-761-76-18",
               address("Zhigulevsk", "Sadovaja", 17, 419)).
    phone_book("D'jakonova", "9-467-96-14",
               address("Sal'sk", "Sovetskaja", 4, 743)).
    phone_book("Sergeev", "3-736-63-59",
               address("Balashov", "Novaja", 44, 802)).
    phone_book("Orlova", "5-871-17-19",
               address("Sorochinsk", "Naberezhnaja", 27, 143)).
    phone_book("L'vov", "6-915-63-73",
               address("Batajsk", "Zarechnaja", 40, 331)).
    phone_book("Frolov", "8-185-42-30",
               address("Nizhnie Sergi", "Zelenaja", 13, 27)).

    owner("Karpov", 1484657,
          car("Volkswagen", "Yellow", "A884AG335")).
    owner("Karpov", 7408751,
          building("Petuhovo", "Tsentral'naja", 50)).
    owner("Karpov", 3882848,
          land_plot(coordinate(-10.530, -39.365), 1292.433)).
    owner("Karpov", 422126,
          ship("Gladiator", "Black")).

    owner("Fedorova", 2094906, car("Peugeout", "Black", "J747JU107")).
    owner("Fedorova", 1339153, ship("REEF Jet", "Blue")).

    owner("Jashina", 773926,
          car("Volkswagen", "Green", "J667SB575")).
    owner("Jashina", 1774898,
          land_plot(coordinate(30.743, 146.001), 99.827)).
    owner("Jashina", 759618, ship("SKAT", "White")).

    owner("Bogdanova", 1637238, car("BMW", "Black", "E207CX015")).
    owner("Bogdanova", 5208570, building("Zhigulevsk", "Shkol'naja", 71)).
    owner("Bogdanova", 2364388,
          land_plot(coordinate(-1.623, 82.491), 3252.474)).
    owner("Bogdanova", 353384,  ship("Rib Tornado", "Silver")).

    owner("Koroleva", 2967590, car("Mitsubishi", "Red", "E333PT682")).
    owner("Koroleva", 842183,  ship("REEF Jet", "Silver")).

    owner("D'jakonova", 2618146, car("BMW", "White", "A514XS854")).
    owner("D'jakonova", 6143979, building("Batajsk", "Novaja", 35)).

    owner("Sergeev", 2960638, car("Mitsubishi", "Yellow", "V230HH761")).

    owner("Orlova", 2642852, car("BMW", "Yellow", "W465LW261")).
    owner("Orlova", 5806373, building("Petuhovo", "Shkol'naja", 7)).
    owner("Orlova", 411479,  ship("REEF Jet", "White")).

    owner("L'vov", 2223430, car("Toyota", "Yellow", "K551YB231")).
    owner("L'vov", 3290281,
          land_plot(coordinate(45.285, -18.754), 1561.308)).
    owner("L'vov", 877502,  ship("Gladiator", "Silver")).

    owner("Frolov", 2158364, car("Volkswagen", "Silver", "O392FC202")).


    sum_int_list(List, Sum) :- in_sum_int_list(List, 0, Sum).
    in_sum_int_list([], Res, Res).
    in_sum_int_list([H | T], Acc, Sum) :- AccIn = Acc + H,
                                          in_sum_int_list(T, AccIn, Sum).

    sum_costs(Name, Total) :- phone_book(Name, _, _),
                              findall(Cost, owner(Name, Cost, _), Costs),
                              sum_int_list(Costs, Total).

goal
    % 1
    % owner(Name, _, Ownership).
    % 2
    % owner(Name, Cost, Ownership).
    % 3
    % sum_costs(Name, Total).

