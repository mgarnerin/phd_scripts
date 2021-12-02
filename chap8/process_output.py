#!/usr/bin/python3
# -*- coding: utf-8 -*-

import os
import argparse
import io
import re
import unicodedata
import numpy
import shutil


def get_spkmap(file):

    gendermap={}

    with open(file,mode="r",encoding="utf-8") as f:
        content=f.read()
        lines=content.split("\n")
        for line in lines[:-1]:
            id, gender = line.split("\t")
            gendermap[id]=gender

    return gendermap



def main():
    
    gmap = get_spkmap("gendermap.csv")


    with io.open("test_other_ds26_3816/result.wrd.txt", mode="r", encoding="utf-8") as f:
        content = f.read()



    rslt = []
    rslt.append("spk\tbook\tutt\thyp\tref\t#C\t#S\t#D\t#I\terr\twc\tWER\tgender")    

    regex = r"id: \((\d+)_(\d+)-(.+)\)\nScores: \(#C #S #D #I\) (\d+) (\d+) (\d+) (\d+)\nREF: (.+)\nHYP:(.+)"

    matches = re.finditer(regex, content, re.MULTILINE)



    for match in matches:
        spk = match.group(1)
        book = match.group(2)
        utt = match.group(3)
        corr = match.group(4)
        subs = match.group(5)
        delt = match.group(6)
        ins = match.group(7)
        ref = match.group(8).strip()
        hyp = match.group(9).strip()
        
        raw_ref = ref.split()
        wc = 0
        for word in raw_ref:
            if "*" not in word:
                wc += 1

        err = int(subs) + int(delt) + int(ins)
        wer = round((err/wc*100),2)

        gender = gmap[spk]

        new_line = spk+"\t"+book+"\t"+utt+"\t"+hyp+"\t"+ref+"\t"+corr+"\t"+subs+"\t"+delt+"\t"+ins+"\t"+str(err)+"\t"+str(wc)+"\t"+str(wer)+"\t"+gender
        rslt.append(new_line)


    with io.FileIO("wer_other_ds26_3816.csv","w") as outf:
        output_content = "\n".join(rslt)
        outf.write(output_content.encode("utf-8"))





if __name__ == '__main__':
        main()
