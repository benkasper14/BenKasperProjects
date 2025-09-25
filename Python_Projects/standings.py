import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import csv

standings = pd.read_csv(r"folder_name")
print(standings)

with open(r'folder_name') as csvfile:
    reader = csv.reader(csvfile)
    next(reader)  # Skip the header
    data = [(row[0], row[1], int(row[2]), int(row[3]), int(row[4]), int(row[5]), int(row[6]),
             int(row[7]), int(row[8])) for row in reader]

# print(data)

# Create arrays
teams = []
sortNum = []

# Create tuples


choice = int(input("1: Wins\n"
                   "2: Losses\n"
                   "3: OTL\n"
                   "4: Points\n"
                   "5: Goals For\n"
                   "6: Goals Against\n"
                   "7: Goal Differential\n"
                   "Enter Choice: "))


def createArrays(index: int):

    # print("HERE")

    teams.clear()
    sortNum.clear()
    for abb in data:
        teams.append(abb[0])
    for num in data:
        sortNum.append(num[index])

    # print(teams)

while choice != 0:
    # Switch choice
    if choice == 1:
        data = sorted(data, key=lambda x: x[2], reverse=True)
        createArrays(2)

        df = pd.DataFrame({'Team': teams, 'Wins': sortNum})
        chart = df.plot(kind='bar', x='Team', y='Wins', color='Green')

    elif choice == 2:
        data = sorted(data, key=lambda x: x[3])
        createArrays(3)

        df = pd.DataFrame({'Team': teams, 'Losses': sortNum})
        chart = df.plot(kind='bar', x='Team', y='Losses', color='Red')

    elif choice == 3:
        data = sorted(data, key=lambda x: x[4])
        createArrays(4)

        df = pd.DataFrame({'Team': teams, 'OTL': sortNum})
        chart = df.plot(kind='bar', x='Team', y='OTL', color='Yellow')

    elif choice == 4:
        data = sorted(data, key=lambda x: x[5], reverse=True)
        createArrays(5)

        df = pd.DataFrame({'Team': teams, 'Points': sortNum})
        chart = df.plot(kind='bar', x='Team', y='Points', color='Green')

    elif choice == 5:
        data = sorted(data, key=lambda x: x[6], reverse=True)
        createArrays(6)

        df = pd.DataFrame({'Team': teams, 'GF': sortNum})
        chart = df.plot(kind='bar', x='Team', y='GF', color='Green')

    elif choice == 6:
        data = sorted(data, key=lambda x: x[7])
        createArrays(7)

        df = pd.DataFrame({'Team': teams, 'GA': sortNum})
        chart = df.plot(kind='bar', x='Team', y='GA', color='Red')

    elif choice == 7:
        data = sorted(data, key=lambda x: x[8], reverse=True)
        createArrays(8)

        df = pd.DataFrame({'Team': teams, 'GD': sortNum})
        chart = df.plot(kind='bar', x='Team', y='GD', color='Blue')

    plt.show()

    choice = int(input("1: Wins\n"
                       "2: Losses\n"
                       "3: OTL\n"
                       "4: Points\n"
                       "5: Goals For\n"
                       "6: Goals Against\n"
                       "7: Goal Differential\n"
                       "0: QUIT\n"
                       "Enter Choice: "))


