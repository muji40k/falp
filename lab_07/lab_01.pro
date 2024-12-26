/*************************************************************************

        Copyright (c) My Company

 Project:  LAB_01
 FileName: LAB_01.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
**************************************************************************/

domains
    name = string
    phone = string
    city = string
    street = string
    mark = string
    color = string
    reg_number = string
    house = integer
    flat = integer
    cost = integer
    addr = address(city, street, house, flat)

predicates
    phone_book(name, phone, addr).
    car_owner(name, mark, color, cost, reg_number).
    model_color_to_owner(mark, color, name, phone, city).

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
    phone_book("Bogdanova", "8-884-07-75",
               address("Batajsk", "Lesnaja", 95, 48)).

    car_owner("Karpov", "Volkswagen", "Yellow", 1484657, "A884AG335").
    car_owner("Fedorova", "Peugeout", "Black", 2094906, "J747JU107").
    car_owner("Jashina", "Volkswagen", "Green", 773926, "J667SB575").
    car_owner("Bogdanova", "BMW", "Black", 1637238, "E207CX015").
    car_owner("Koroleva", "Mitsubishi", "Red", 2967590, "E333PT682").
    car_owner("D'jakonova", "BMW", "White", 2618146, "A514XS854").
    car_owner("Sergeev", "Mitsubishi", "Yellow", 2960638, "V230HH761").
    car_owner("Orlova", "BMW", "Yellow", 2642852, "W465LW261").
    car_owner("L'vov", "Toyota", "Yellow", 2223430, "K551YB231").
    car_owner("Frolov", "Volkswagen", "Silver", 2158364, "O392FC202").

    model_color_to_owner(Mark, Color, Name, Number, City)
    :- car_owner(Name, Mark, Color, _, _),
       phone_book(Name, Number, address(City, _, _, _)).

goal
    model_color_to_owner("BMW", "Black", Name, Number, City).

