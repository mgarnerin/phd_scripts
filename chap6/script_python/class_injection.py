#!/usr/bin/python3
# -*- coding: utf-8 -*-

def get_episode_id(episode, hash):
    return hash[episode]


def get_id_show(id_episode, hash):
    return hash[id_episode]


def get_spk_class(spk_name, gender, id_show, hash):
    if (spk_name, gender, id_show) not in hash.keys():
        print('SPEAKER MISSING',spk_name,gender,id_show)
        return 1
    else:
        return hash_class[(spk_name, gender, id_show)]

hash_episode = {}
f = open("./../data/mapping_files/id_episode2filename.csv", mode="r", encoding="utf-8")
content = f.read()
f.close()

lines = content.split("\n")
for line in lines[:-1]:
    id_episode, filename = line.split(";")
    hash_episode[filename] = id_episode

hash_show = {}
f = open("./../data/mapping_files/id_episode2id_show.csv", mode="r", encoding="utf-8")
content = f.read()
f.close()

lines = content.split("\n")
for line in lines[:-1]:
    id_episode, id_show = line.split(";")
    hash_show[id_episode] = id_show


hash_class = {}
f = open("./../data/mapping_files/spk2class_test.csv", mode="r", encoding="utf-8")
content = f.read()
f.close()

lines = content.split("\n")
for line in lines[1:-1]:
    spk_name, gender, id_show, spk_class = line.split("\t")
    hash_class[(spk_name, gender, id_show)] = spk_class


f = open("./../data/eval_data.csv", mode="r", encoding="utf-8")
content = f.read()
f.close()

outputfile = open("./../data/eval_data_wclass.csv", mode="w", encoding="utf-8")
outputfile.write("episode,speaker,start_time,end_time,trans,hyp,mistakes,WER,type,native,gender,spk_role,id_show\n")
lines = content.split("\n")
for line in lines[1:-1]:
    cols = line.split(',')
    episode = cols[0]
    id_episode = get_episode_id(episode,hash_episode)
    id_show = get_id_show(id_episode, hash_show)

    spk_name = cols[1]
    gender = cols[10]
    spk_class = get_spk_class(spk_name, gender, id_show, hash_class)

    outputfile.write(','.join(cols[0:11])+','+str(spk_class)+","+str(id_show)+"\n")
