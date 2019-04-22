def getInputFileContent(fileName):
    inputFile = open(fileName, 'r')
    fileContent = inputFile.readlines()
    header = fileContent.pop(0)
    familySet = set()
    genusSet = set()
    speciesSet = set()
    for content in fileContent:
        lineContent = content.split(",")
        familySet.add(lineContent[22])
        genusSet.add(lineContent[23])
        speciesSet.add(lineContent[24])

    # use index in family
    i = 1
    familyIndex = {}
    for family in familySet:
        familyIndex[family] = str(i)
        i = i+1

    i = 1
    genusIndex = {}
    for genus in genusSet:
        genusIndex[genus] = str(i)
        i = i+1

    i = 1
    speciesIndex = {}
    for species in speciesSet:
        speciesIndex[species] = str(i)
        i = i+1

    print("family: ", familyIndex)
    print("genus: ", genusIndex)
    print("species: ", speciesIndex)

    return header, familyIndex, genusIndex, speciesIndex, fileContent


def getNewContent(familyIndex, genusIndex, speciesIndex, originalContent):
    newContent = []
    for content in originalContent:
        lineContent = content.split(",")
        lineContent[22] = familyIndex[lineContent[22]]
        lineContent[23] = genusIndex[lineContent[23]]
        lineContent[24] = speciesIndex[lineContent[24]]
        newRow = ",".join(lineContent)
        newContent.append(newRow)

    return newContent


def writeToNewFile(header, newContent, newFileName):
    f = open(newFileName, 'w')
    f.write(header+"\n")
    for content in newContent:
        f.write(content+"\n")
    f.close()
    print('writing done')
    return


def main():
    inputFile = "/Users/sxs2561/Documents/AcademicAssignments/stat_557/project_3/datamining_project_3/data/Frogs_MFCCs.csv"
    outputFile = "/Users/sxs2561/Documents/AcademicAssignments/stat_557/project_3/datamining_project_3/data/Frogs_MFCCs_FINAL.csv"

    header, familyIndex, genusIndex, speciesIndex, fileContent = getInputFileContent(inputFile)
    newContent = getNewContent(familyIndex, genusIndex, speciesIndex, fileContent)
    writeToNewFile(header=header, newContent=newContent, newFileName=outputFile)


if __name__ == '__main__':
    main()
