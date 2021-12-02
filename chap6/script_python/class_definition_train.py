import os


def _get_filename(path):
    while '.' in path:
        path = os.path.splitext(os.path.basename(path))[0]
    return path


def extract_classes(filename, i):
    speakers = {}
    total_length = 0
    total_turn = 0

    f = open(filename, mode="r", encoding="utf8")
    content = f.read()
    lines = content.split("\n")

    for line in lines[1:-1]:
        #print(isinstance(line, str))
        # id_turn;start_time;end_time;name;gender;id_episode;id_show
        #3892;4.968;7.369;Guillaume_VANHEMS;male;38;8
        id_turn, start_time, end_time, name, gender, id_episode, id_show = line.split(";")
        key=(name, gender)
        if key in speakers.keys():
           speakers[key]["count"] += 1
           length=float(end_time)-float(start_time)
           speakers[key]["length"] += length
        else:
            speakers[key] = {}
            speakers[key]["count"] = 1
            length = float(end_time) - float(start_time)
            speakers[key]["length"] = length

        total_length += length
        total_turn += 1
    f.close()

    output_filename = "./../data/train_data_shows/"+_get_filename(filename)+"_speaker_class.csv"

    fo = open(output_filename, mode="w", encoding="utf-8")
    fo.write("spk_name;gender;turn_count;total_length;spk_class;id_show\n")
    for (id, gender) in speakers.keys():

        count = speakers[(id,gender)]["count"]
        length = speakers[(id,gender)]["length"]
        if count < 0.01*total_turn:
            if length < 0.01*total_length:
                spk_class = "1"
            else:
                spk_class = "3"
        else:
            if length < 0.01*total_length:
                spk_class = "2"
            else:
                spk_class = "4"

        fo.write(id+";"+gender+";"+str(count)+";"+str(length)+";"+spk_class+";"+str(i)+"\n")

    fo.close()


def main():
    for i in [2,8,9,10,13,19,22,26]:
        filename = "./../data/train_data_shows/train_acoustic_show"+str(i)+".csv"
        extract_classes(filename, i)

    roles={}

    for file in os.listdir():
        if "train_acoustic_show" in file:
            f = open(file,mode="r",encoding="utf-8")
            content = f.read()
            f.close()
            lines = content.split("\n")
            for line in lines[1:-1]:
                spk_name,gender,total_turn,total_length,spk_class,id_show = line.split(";")
                roles[(spk_name,id_show,gender,total_length,total_turn)]=spk_class
    f = open("./../data/spk_all_info_train.csv",mode="w",encoding="utf-8")
    f.write('spk_name\tid_show\tgender\tspk_class\ttotal_length\ttotal_count\n')
    for s in roles:
      f.write(s[0]+"\t"+s[1]+"\t"+s[2]+"\t"+roles[s]+"\t"+s[3]+"\t"+s[4]+"\n")



if __name__ == '__main__':
    main()
