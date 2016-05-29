#variable 5, annual domestic harvest (H)
#Var5= (Calculaton DM) *1000
#DM= total industrial RW + bark + fuelwood
#DM= DI + DO + DN


#DI= industrial roundwood production as C flow
#calculated by `parameters&results'! $O$17 * Control!C

#PRO17 is calculated by M13 * M17 / 10^6
#M13= dry weight of solid wood products = .907185 Mg/odt
#M17= solid wood products carbon factor= .5
PRO17 <- 0.0000004535925 ##SWP, odt -> Tg/c

#control C= roundwood production..same as USA!N

#for years before 1950=
#`Hair_1963_Table2_adj'AL__ *Ince_Table4! N$5  *1000
#1920 AL26
#1930 AL36
#1940 AL46
#hair1963AL= apparent consumption of other products 
#Ince_table4! N$5 = conversion factor to od tons of wood== 14.897
InceN5 <- 14.89675 #### pulpwood (other industrial products) 1000ft^3 to od tons

#for years before 1965=
#Ulrich_Table4_adj! Y __ * Ince_table4! N$5   *1000
#1950= Y9
#1960= Y19
#1961= Y20
#1962= Y21
#1963 = Y22
#1964= Y23
#Ulrich_Table4_adj Y= other industrial products...production and consumption



#for years before 2021...1965 to 2020 =
#Howard_table5a!T __ * Ince_Table4! N$5     *10000
#howard_table5a  T= other industrial products, production and consumption(million cubic ft, roundwood equivalent)


#for years before 2050 =
#Ince_Table1! M __ * Ince_Table4! N$5    *1000
#Ince_table1  M = other industrial products (million ft^3)




###########
#Calculation DO= fuelwood
#calculated by `parameters&results'! $O$17 * Control!G

#Control G= roundwood fuelwood...same as USA G

#for years before 1950
#(`Ulrich_table_5_adj`AB_-*Ince_Table4! V$5 + `Ulrich_table6_adj! AB__ *Ince_table4! W$5)  /  (Ulrich_table5_adj! AB __ + Ulrich_table6_adj! AB__) * Hair_1963_table2_adj! AM__  * 1000

#Ulrich_table5_adj AB= fuelwood production and consumption 
#Ince_table4!  V$5 = conversion factor to od tons of wood==  13.865
InceV5 <- 13.865 ### Softwood roundwood 1000 Cu ft



