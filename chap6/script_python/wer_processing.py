#!/usr/bin/python3
# -*- coding: utf-8 -*-

import os

def word_count(input):

    """
    Count the number of words in a string. Words are defined as a sequence of characters between two spaces.
    """

    words = input.split(" ")
    return len(words)

def main() :
    """
    Compute the WER over each episode by speakerself.
    Return a .csv file containing the episode name, the speaker name and gender, total of words, total of mistakes of the ASR system and WER.
    """

    turns = {}

    f = open("./../data/eval_data_wclass.csv", mode="r", encoding="utf-8")
    content = f.read()
    lines = content.split("\n")
    f.close()

    #episode,speaker,start_time,end_time,trans,hyp,mistakes,WER,type,native,gender,spk_role,id_show

    for line in lines[1:-1]:
        line = line.split(",")
        episode = line[0]
        spk = line[1]
        gender = line[10]
        trans = line[4]
        wc = word_count(trans)
        mistakes = int(line[6])
        spk_class = line[11]
        id_show = line[12]


        if (episode,spk,gender,spk_class) in turns.keys() :
            turns[(episode,spk,gender,spk_class)]["wc"] += wc
            turns[(episode,spk,gender,spk_class)]["mistakes"] += mistakes

        else :
            turns[(episode,spk,gender,spk_class)] = {}
            turns[(episode,spk,gender,spk_class)]["episode"] = episode
            turns[(episode,spk,gender,spk_class)]["spk"] = spk
            turns[(episode,spk,gender,spk_class)]["wc"] = wc
            turns[(episode,spk,gender,spk_class)]["mistakes"] = mistakes
            turns[(episode,spk,gender,spk_class)]["id_show"] = id_show

    f = open("./../data/wer_by_episode.csv", mode="w", encoding="utf-8")
    f.write("id_show\tepisode\tspeaker\tgender\tspk_class\twc\tmistakes\tWER\n")
    for (ep, spk, gender, spk_class) in turns.keys():
        wc = turns[(ep, spk, gender, spk_class)]["wc"]
        mistakes = turns[(ep, spk, gender, spk_class)]["mistakes"]
        id_show = turns[(ep, spk, gender, spk_class)]["id_show"]
        wer = mistakes/wc
        line = "\t".join((id_show, ep, spk, gender, spk_class, str(wc), str(mistakes), str(round(wer, 2))))
        line += "\n"
        f.write(line)
    f.close()

if __name__ == '__main__':
    main()
