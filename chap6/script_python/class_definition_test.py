import os


def _get_filename(path):
    while '.' in path:
        path = os.path.splitext(os.path.basename(path))[0]
    return path


def get_episode_id(episode, hash):
    return hash[episode]


def get_id_show(id_episode, hash):
    return hash[id_episode]



def extract_classes(filename):

    hash_episode = {}
    f = open("./../data/mapping_files/id_episode2filename.csv", mode="r", encoding="utf-8")
    content = f.read()
    f.close()

    lines = content.split("\n")
    for line in lines[:-1]:
        id_episode, file_name = line.split(";")
        hash_episode[file_name] = id_episode

    hash_show = {}
    f = open("./../data/mapping_files/id_episode2id_show.csv", mode="r", encoding="utf-8")
    content = f.read()
    f.close()

    lines = content.split("\n")
    for line in lines[:-1]:
        id_episode, id_show = line.split(";")
        hash_show[id_episode] = id_show


    shows = {}

    f = open(filename, mode="r", encoding="utf8")
    content = f.read()
    lines = content.split("\n")

    for line in lines[1:-1]:
        #print(isinstance(line, str))
        #episode,speaker,start_time,end_time,trans,hyp,mistakes,WER,type,native,gender
        episode,speaker,start_time,end_time,trans,hyp,mistakes,WER,type,native,gender = line.split(",")

        id_episode = get_episode_id(episode, hash_episode)
        id_show = get_id_show(id_episode, hash_show)

        #print(id_show)

        if id_show not in shows.keys():
            shows[id_show]={}
            shows[id_show]["speakers"]={}
            shows[id_show]["total_length"] = 0
            shows[id_show]["total_turn"] = 0

        key = (speaker, gender)

        if key in shows[id_show]["speakers"].keys():
           shows[id_show]["speakers"][key]["count"] += 1
           length=float(end_time)-float(start_time)
           shows[id_show]["speakers"][key]["length"] += length
        else:
            shows[id_show]["speakers"][key] = {}
            shows[id_show]["speakers"][key]["count"] = 1
            length = float(end_time) - float(start_time)
            shows[id_show]["speakers"][key]["length"] = length

        shows[id_show]["total_length"] += length
        shows[id_show]["total_turn"] += 1
    f.close()

    output_filename = "./../data/spk_all_info_test.csv"

    fo = open(output_filename, mode="w", encoding="utf-8")
    fo.write("spk_name;gender;turn_count;total_length;spk_class;id_show\n")
    for id_show in shows.keys():
        for (id, gender) in shows[id_show]["speakers"].keys():

            count = shows[id_show]["speakers"][(id,gender)]["count"]
            length = shows[id_show]["speakers"][(id,gender)]["length"]
            if count < 0.01*shows[id_show]["total_turn"]:
                if length < 0.01*shows[id_show]["total_length"]:
                    spk_class = "1"
                else:
                    spk_class = "3"
            else:
                if length < 0.01*shows[id_show]["total_length"]:
                    spk_class = "2"
                else:
                    spk_class = "4"

            fo.write(id+";"+gender+";"+str(count)+";"+str(length)+";"+spk_class+";"+id_show+"\n")

    fo.close()


def main():
    extract_classes("./../data/eval_data.csv")

    roles={}

    f = open("./../data/spk_all_info_test.csv",mode="r",encoding="utf-8")
    content = f.read()
    f.close()
    lines = content.split("\n")
    for line in lines[1:-1]:
        spk_name,gender,total_turn,total_length,spk_class,id_show = line.split(";")
        if (spk_name,gender,id_show) not in roles.keys():
            roles[(spk_name,gender,id_show)]=spk_class
    f = open("./../data/mapping_files/spk2class_test.csv",mode="w",encoding="utf-8")
    f.write('spk_name\tgender\tid_show\tspk_class\n')
    for (spk_name,gender,id_show) in roles:
        f.write("\t".join((spk_name,gender,id_show,roles[(spk_name,gender,id_show)]))+"\n")


if __name__ == '__main__':
    main()
