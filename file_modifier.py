
def getInputFileContent(fileName):
    inputFile = open(fileName, 'r')
    fileContent = inputFile.readlines()
    header = fileContent.pop(0)
    labels = set()
    for content in fileContent:
        lineContent = content.split(",")
        # print(lineContent[0])
        labels.add(lineContent[0])

    # print(labels)
    i = 1
    classIndex = {}
    for label in labels:
        classIndex[label] = str(i)
        i = i+1

    print(classIndex)

    return header, classIndex, fileContent


def getNewContent(classIndex, originalContent):
    newContent = []
    for content in originalContent:
        lineContent = content.split(",")
        lineContent[0] = classIndex[lineContent[0]]
        newRow = ",".join(lineContent)
        newContent.append(newRow)

    print(newContent[0])
    return newContent


def writeToNewFile(header, newContent, newFileName):
    f = open(newFileName, 'w')
    f.write(header+"\n")
    for content in newContent:
        f.write(content+"\n")
    f.close()
    print('writing done')
    return


def hello():
    s1 = "a,b,c"
    s2 = s1.split(",")
    s3 = ",".join(s2)
    print(s3)


def main():
    inputFile = "data/training_k.csv"
    outputFile = "data/training_k_final.csv"
    header, classIndexes, originalContent = getInputFileContent(fileName=inputFile)
    newContent = getNewContent(classIndexes, originalContent)
    writeToNewFile(header=header, newContent=newContent, newFileName=outputFile)


if __name__ == '__main__':
    main()
