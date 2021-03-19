import re
import os
os.getcwd()
speaker_words = {}
speaker_pattern = re.compile(r'^(\w+?).(.*)$')

with open("data/male_white/c115_kavanaugh.pdf", "r") as f:
        lines = f.readlines()
        current_speaker = None
        for line in lines:
                line = line.strip()
                match = speaker_pattern.match(line)
                if match is not None:
                        current_speaker = match.group(1)
                        line = match.group(2).strip()
                        if current_speaker not in speaker_words.keys():
                                speaker_words[current_speaker] = []
                if current_speaker:
                        # you may want to do some sort of punctuation filtering too
                        words = [word.strip() for word in line.split(' ') if len(word.strip()) > 0]
                        speaker_words[current_speaker].extend(words)

print(speaker_words)