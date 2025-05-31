# I wrote this program in year 7 or 8.
# I would like to think that my programming ability has advanced somewhat since then.
import random

print("""Dungeon Quest
The RPG
Created by Aidan Hall
 """)
gameExit = False

inventory = ["weak healing potion"]
weapon = "short sword"
armour = "padded armour"
level = 1
exp = 0
HPc = 10
totalatk = 0
totaldef = 0
item = 0
itemuse = 0
unequip = 0
equiplocation = 0
action = 0
dunglevel = 1
dungroomtype = 0
monsterrace = 0
monsteratr = 0
monsterlvl = 0
inroom = False
fighting = False
dungroomc = 0
# Variable defining.



while gameExit == False:
    expmax = (level * 20)
    HPm = (level * 10)
    if HPc > HPm:
        HPc = HPm
    attack = (level * 3)
    defence = (level * 2)
    # Add new weapons and armour here.
    if weapon == "short sword":
        totalatk = attack + 1
    if weapon == "small axe":
        totalatk = attack + 3
    if weapon == "hero sword":
        totalatk = attack + 5
    if armour == "padded armour":
        totaldef = defence + 1
    if armour == "chain mail":
        totaldef = defence + 3
    if armour == "mythril":
        totaldef = defence + 5

    dungroomf = (dunglevel * 5)
    dungrooml = (dungroomf - dungroomc)
    # Stat and dungeon calculations.
    if HPc <= 0:
        print("You died!")
        print("Game over!")
        quit()
    if inroom == False:
        print ("Actions:")
        print ("stats, progress, explore, item, quit, inventory")
        action = input("What will you do? ")
        if action == "progress":
            print("You are at floor", dunglevel, "of the dungeon.")
            print("You have cleared", dungroomc, "room(s) on this floor.")
            print("You have", dungrooml, "room(s) remaing.")
        if action == "stats":
            print("Level:", level)
            print("EXP:", exp, "/", expmax)
            print("HP:", HPc, "/", HPm)
            print("Attack:", totalatk, "(", attack, ")")
            print("Defence:", totaldef, "(", defence, ")")
            print("Weapon:", weapon)
            print("Armour:", armour)
        if action == "quit":
            print("Thanks for playing.")
            quit()
        if action == "explore":
            print("You find a new room.")
            enter = input("Will you enter the room? ")
            if enter == "no":
                print("You decide to wait.")
            if enter == "yes":
                print("You enter the room.")
                inroom = True
        if action == "item":
            print(inventory)
            item = input("Which item do you want to use? ")
            if inventory.count(item) == 0:
                print("You don't have that item.")
                continue
            print ("Uses: equip, use, discard, cancel")
            itemuse = input("What do you want to do with it? ")
            if itemuse == "discard":
                inventory.remove(item)
            if itemuse == "use":
                if item == "weak healing potion":
                    HPc += 5
                    print("Restored 5 HP.")
                    inventory.remove(item)
                elif item == "standard healing potion":
                    HPc += 10
                    print("Restored 10 HP.")
                    inventory.remove(item)
                else:
                    print("You can't use that item!")
            if itemuse == "equip":
                print ("weapon:", weapon)
                print ("armour:", armour)
                equiplocation = input("Where do you want to equip this? ")
                if equiplocation == "weapon":
                    # Add new weapons here.
                    if item == "short sword" or item == "small axe" or item == "hero sword":
                        unequip = weapon
                        weapon = item
                        inventory.append(unequip)
                        inventory.remove(item)
                        print(item, "equipped.")
                    else:
                        print("You can't equip that as a weapon!")
                if equiplocation == "armour":
                    # Add new armour here.
                    if item == "padded armour" or item == "chain mail" or item == "mythril":
                        unequip = armour
                        armour = item
                        inventory.append(unequip)
                        inventory.remove(item)
                        print (item, "equipped.")
                    else:
                        print("You can't equip that as armour!")
        if action == "cheat":
            level += 10
            HPc = HPm
            print ("You dirty cheater!")
        if action == "inventory":
            print (inventory)
        x = input("...")


    if inroom == True:
        dungroomtype = random.randint(1,5)
        if dungroomtype < 4:
            dungroomtype = "monster"
        if dungroomtype == 4:
            dungroomtype = "trap"
        if dungroomtype == 5:
            dungroomtype = "treasure"
        if dungrooml == 1:
            dungroomtype = "boss"
        print("This is a", dungroomtype, "room.")
        x = input("...")

        if dungroomtype == "monster":
            monsterrace = random.randint(1,100)
            if monsterrace < 51:
                monsterrace = "orc"
            elif monsterrace > 50:
                monsterrace = "troll"
            monsteratr = random.randint(1,100)
            if monsteratr < 11:
                monsteratr = "burly"
            elif monsteratr > 90:
                monsteratr = "angry"
            else:
                monsteratr = "normal"
            monsterlvl = random.randint(dunglevel, dunglevel + 1)
            print ("You encountered a level ", monsterlvl, monsteratr, monsterrace, "!") #Monster type generation.
            x = input ("...")
            if monsterrace == "orc":
                monsterHPm = monsterlvl * 5
                monsteratk = monsterlvl * 2
                monsterdef = monsterlvl * 2
            if monsterrace == "troll":
                monsterHPm = monsterlvl * 7
                monsteratk = monsterlvl
                monsterdef = monsterlvl * 3
            if monsteratr == "burly":
                monsterHPm += 2
            if monsteratr == "angry":
                monsteratk += 2
            monsterHPc = monsterHPm
            print("The level", monsterlvl, monsteratr, monsterrace, "'s stats:")
            print("HP:",monsterHPm)
            print("Attack:", monsteratk)
            print("Defence:", monsterdef)  # Monster stat generation
            x = input("...")
            fighting = True
            while fighting == True:
                print("Your turn.")
                print("Monster's HP:", monsterHPc, "/", monsterHPm)
                print("Your HP:", HPc, "/", HPm)
                x = input("...")
                print("Actions:")
                print("attack, stats, inventory, item, quit")
                action = input("What will you do? ")
                if action == "inventory":
                    print(inventory)
                if action == "stats":
                    print("Level:", level)
                    print("EXP:", exp, "/", expmax)
                    print("HP:", HPc, "/", HPm)
                    print("Attack:", totalatk, "(", attack, ")")
                    print("Defence:", totaldef, "(", defence, ")")
                    print("Inventory:", inventory)
                    print("Weapon:", weapon)
                    print("Armour:", armour)
                if action == "quit":
                    print("Thanks for playing.")
                    quit()
                if action == "item":
                    print(inventory)
                    item = input("Which item do you want to use? ")
                    if inventory.count(item) == 0:
                        print("You don't have that item.")
                        continue
                    print("Uses: equip, use, discard, cancel")
                    itemuse = input("What do you want to do with it? ")
                    if itemuse == "discard":
                        inventory.remove(item)
                    if itemuse == "use":
                        if item == "weak healing potion":
                            HPc += 5
                            print("Restored 5 HP.")
                            inventory.remove(item)
                        elif item == "standard healing potion":
                            HPc += 10
                            print("Restored 10 HP.")
                            inventory.remove(item)
                        else:
                            print("You can't use that item!")
                    elif itemuse == "equip":
                        print("weapon:", weapon)
                        print("armour:", armour)
                        equiplocation = input("Where do you want to equip this? ")
                        if equiplocation == "weapon":
                            # Add new weapons here.
                            if item == "short sword" or item == "small axe" or item == "hero sword":
                                unequip = weapon
                                weapon = item
                                inventory.append(unequip)
                                inventory.remove(item)
                                print(item, "equipped.")
                            else:
                                print("You can't equip that as a weapon!")
                        if equiplocation == "armour":
                            # Add new armour here.
                            if item == "padded armour" or item == "chain mail" or item == "mythril":
                                unequip = armour
                                armour = item
                                inventory.append(unequip)
                                inventory.remove(item)
                                print(item, "equipped.")
                            else:
                                print("You can't equip that as armour!")
                if action == "attack":
                    print("You attack!")
                    x = input("...")
                    damage = (totalatk - monsterdef)
                    crit = random.randint(1,100)
                    if damage < 1:
                        damage = 1
                    if crit > 90:
                        damage *= 2
                        print("Critical hit!")
                    damage = round(damage, 0)
                    print ("You do", damage, "damage!")
                    monsterHPc -= damage
                    if monsterHPc <= 0:
                        break
                    print("Monster's HP:", monsterHPc, "/", monsterHPm)
                x = input("...")
                print("Monster's turn!")
                print("Monster attacks!")
                x = input("...")
                damage = (monsteratk - totaldef)
                crit = random.randint(1, 100)
                if crit > 90:
                    damage *= 2
                    print("Critical hit!")
                if damage < 1:
                    damage = 1
                damage = round(damage, 0)
                print("The monster does", damage, "damage!")
                HPc -= damage
                if HPc <= 0:
                    print("You were defeated.")
                    x = input("Thanks for playing.")
                    quit()
                x = input ("...")

            print("You defeated the monster!")
            expdrop = random.randint(monsterlvl,monsterlvl + 2)
            expdrop *= round(monsterlvl/level , 0)
            print("You got", expdrop, "EXP points!")
            exp += expdrop
            print ("EXP:", exp,"/",expmax)
            x = input("...")
            if exp >= expmax:
                print("Level up!")
                exp = 0
                level += 1
                print("Level:" , level)
                x = input("...")
            dungroomc += 1
            treasures = random.randint(monsterlvl, monsterlvl + 1)
            x = input("The monster dropped treasure!")
            for i in range (treasures):
                trenum = i + 1
                print("Treasure", trenum,":")
                treasure = random.randint(1,100)
                if treasure < 21:
                    treasure = "chain mail"
                elif treasure > 20 and treasure <= 50:
                    treasure = "weak healing potion"
                elif treasure > 50 and treasure < 71:
                    treasure = "small axe"
                elif treasure > 70 and treasure < 81:
                    treasure = "strong healing potion"
                elif 80 < treasure < 91:
                    treasure = "mythril"
                elif 90 < treasure:
                    treasure = "hero sword"
                print(treasure)
                inventory.append(treasure)
                x = input("...")

            inroom = False

        if dungroomtype == "trap":
            trap = random.randint(1, 10)
            damage = round(HPc/trap, 0)
            x = input("It's a trap!")
            print ("The trap did", damage, "damage!")
            HPc -= damage
            print("HP:", HPc, "/", HPm)
            x = input("...")
            dungroomc += 1
            inroom = False

        if dungroomtype == "treasure":
            treasures = random.randint(dunglevel + 1, dunglevel + 3)
            print ("You find", treasures, "treasure(s).")
            for i in range (treasures):
                trenum = i + 1
                print("Treasure", trenum,":")
                treasure = random.randint(1,100)
                if treasure < 21:
                    treasure = "chain mail"
                elif treasure > 20 and treasure <= 50:
                    treasure = "weak healing potion"
                elif treasure > 50 and treasure < 71:
                    treasure = "small axe"
                elif treasure > 70 and treasure < 81:
                    treasure = "strong healing potion"
                elif 80 < treasure < 91:
                    treasure = "mythril"
                elif 90 < treasure:
                    treasure = "hero sword"
                print(treasure)
                inventory.append(treasure)
                x = input("...")
            dungroomc += 1
            inroom = False
        if dungroomtype == "boss":
            monsterrace = "draco lord"
            monsterlvl = dunglevel
            print ("You encountered a level", monsterlvl, monsterrace, "!")
            print("This is stupid!")
            monsterHPm = (monsterlvl * 20)
            monsteratk = (monsterlvl * 5)
            monsterdef = (monsterlvl * 4)
            monsterHPc = monsterHPm
            print("The level", monsterlvl, monsterrace, "'s stats:")
            print("HP:", monsterHPm)
            print("Attack:", monsteratk)
            print("Defence:", monsterdef)  # Monster stat generation
            x = input("...")
            fighting = True
            while fighting == True:
                print("Your turn.")
                print("Boss's HP:", monsterHPc, "/", monsterHPm)
                print("Your HP:", HPc, "/", HPm)
                x = input("...")
                print("Actions:")
                print("attack, stats, inventory, item, quit")
                action = input("What will you do? ")
                if action == "inventory":
                    print(inventory)
                if action == "stats":
                    print("Level:", level)
                    print("EXP:", exp, "/", expmax)
                    print("HP:", HPc, "/", HPm)
                    print("Attack:", totalatk, "(", attack, ")")
                    print("Defence:", totaldef, "(", defence, ")")
                    print("Inventory:", inventory)
                    print("Weapon:", weapon)
                    print("Armour:", armour)
                if action == "quit":
                    print("Thanks for playing.")
                    quit()
                if action == "item":
                    print(inventory)
                    item = input("Which item do you want to use? ")
                    if inventory.count(item) == 0:
                        print("You don't have that item.")
                        continue
                    print("Uses: equip, use, discard, cancel")
                    itemuse = input("What do you want to do with it? ")
                    if itemuse == "discard":
                        inventory.remove(item)
                    if itemuse == "use":
                        if item == "weak healing potion":
                            HPc += 5
                            print("Restored 5 HP.")
                        elif item == "standard healing potion":
                            HPc += 10
                            print("Restored 10 HP.")
                            inventory.remove(item)
                        else:
                            print("You can't use that item!")
                    elif itemuse == "equip":
                        print("weapon:", weapon)
                        print("armour:", armour)
                        equiplocation = input("Where do you want to equip this? ")
                        if equiplocation == "weapon":
                            # Add new weapons here.
                            if item == "short sword" or item == "small axe" or item == "hero sword":
                                unequip = weapon
                                weapon = item
                                inventory.append(unequip)
                                inventory.remove(item)
                                print(item, "equipped.")
                            else:
                                print("You can't equip that as a weapon!")
                        if equiplocation == "armour":
                            # Add new armour here.
                            if item == "padded armour" or item == "chain mail" or item == "mythril":
                                unequip = armour
                                armour = item
                                inventory.append(unequip)
                                inventory.remove(item)
                                print(item, "equipped.")
                            else:
                                print("You can't equip that as armour!")
                if action == "attack":
                    print("You attack!")
                    x = input("...")
                    damage = (totalatk - monsterdef)
                    crit = random.randint(1, 100)
                    if damage < 1:
                        damage = 1
                    if crit > 90:
                        damage *= 2
                        print("Critical hit!")
                    damage = round(damage, 0)
                    print("You do", damage, "damage!")
                    monsterHPc -= damage
                    if monsterHPc <= 0:
                        break
                    print("Monster's HP:", monsterHPc, "/", monsterHPm)
                x = input("...")
                print("Boss's turn!")
                print("Boss attacks!")
                x = input("...")
                damage = (monsteratk - totaldef)
                crit = random.randint(1, 100)
                if crit > 90:
                    damage *= 2
                    print("Critical hit!")
                if damage < 1:
                    damage = 1
                damage = round(damage, 0)
                print("The boss does", damage, "damage!")
                HPc -= damage
                if HPc <= 0:
                    print("You were defeated.")
                    x = input("Thanks for playing.")
                    quit()
                x = input("...")
            print ("You defeated the boss!")
            x = input("...")
            expdrop = (monsterlvl * 500)
            print("You got", expdrop, "EXP points!")
            exp += expdrop
            print("EXP:", exp, "/", expmax)
            x = input("...")
            if exp >= expmax:
                print("Level up!")
                exp = 0
                level += 1
                print("Level:", level)
                x = input("...")
            dungroomc += 1
            treasures = (dunglevel * 5)
            x = input("The boss dropped treasure!")
            for i in range(treasures):
                trenum = i + 1
                print("Treasure", trenum, ":")
                treasure = random.randint(1, 100)
                if treasure < 21:
                    treasure = "chain mail"
                elif treasure > 20 and treasure <= 50:
                    treasure = "weak healing potion"
                elif treasure > 50 and treasure < 71:
                    treasure = "small axe"
                elif treasure > 70 and treasure < 81:
                    treasure = "strong healing potion"
                elif 80 < treasure < 91:
                    treasure = "mythril"
                elif 90 < treasure:
                    treasure = "hero sword"
                print(treasure)
                inventory.append(treasure)
                x = input("...")
            print("You've reached the end of dungeon level", dunglevel, "!")
            x = input("...")
            dunglevel += 1
            print("Welcome to level", dunglevel, "of the dungeon.")
            x = input("...")
            dungroomc = 0
            inroom = False
