import os

SIDE_1 = "gtm"
SIDE_2 = "oua"
SIDE_3 = "cin"
SIDE_4 = "per"


def is_valid(w, face, status):
    if len(w) < 3:
        return False, 0
    prev_side = -1
    value = 0
    new_status = []
    for e in status:
        new_status.append(e[:])
    for c in w:
        for side in range(4):
            if side != prev_side and c in face[side]:
                prev_side = side
                c_idx = face[side].index(c)
                if not new_status[side][c_idx]:
                    new_status[side][c_idx] = True
                    value += 1
                break
        else:
            return False, 0
    return True, value


def covers(lst, flat):
    return set("".join(lst)) == flat


def words():
    lst = []
    curr_directory = os.path.dirname(os.path.realpath('__file__'))
    dictionary_path = os.path.join(curr_directory, '\words.txt')
    dictionary_path = os.path.abspath(os.path.realpath(dictionary_path))
    with open(dictionary_path, 'r') as file:
        for row in file:
            lst.append(row.strip())
    return lst


def main():
    face_letters = [list(SIDE_1), list(SIDE_2), list(SIDE_3), list(SIDE_4)]
    flat_list = set([item for sublist in face_letters for item in sublist])
    face_status = [[False]*3 for _ in range(4)]
    universe = []

    for word in words():
        ok, v = is_valid(word, face_letters, face_status)
        if ok:
            universe.append(word)

    for w0 in universe:
        if covers([w0], flat_list):
            print("SINGLE WORD ---> " + w0)

    for w1 in universe:
        for w2 in universe:
            if w1[-1] == w2[0] and covers([w1, w2], flat_list):
                print(" + ".join([w1, w2]))


if __name__ == '__main__':
    main()
