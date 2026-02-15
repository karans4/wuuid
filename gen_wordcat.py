#!/usr/bin/env python3
"""Parse BIP39 wordlist.h and categorize words as noun/adj/verb, emit wordcat.h"""

import re

WORDLIST_H = "/home/k-with-claude/wuuid/wordlist.h"
OUTPUT_H = "/home/k-with-claude/wuuid/wordcat.h"

# Explicit sets
ADJ_SET = {"abstract","absurd","acoustic","actual","afraid","angry","annual","arctic","awesome","awful","basic","best","better","big","bitter","blind","blue","bleak","brave","brief","bright","brisk","broken","brown","brute","busy","calm","capable","casual","certain","cheap","chronic","civil","clean","clever","close","clumsy","cold","common","complete","cool","correct","cozy","crazy","crisp","cruel","curious","cute","damp","dark","dead","dear","decent","deep","digital","direct","dizzy","double","dry","dumb","dynamic","eager","early","easy","electric","elegant","elite","empty","endless","entire","equal","eternal","evil","exact","exotic","extra","false","famous","fancy","fatal","few","final","fine","first","fit","flat","fluid","fragile","free","frequent","fresh","front","frozen","fun","funny","general","gentle","genuine","giant","glad","gloomy","gold","good","great","green","grim","half","happy","hard","harsh","heavy","high","hollow","huge","human","humble","hungry","hybrid","idle","ill","illegal","immune","indoor","initial","inner","innocent","insane","intact","jealous","keen","kind","large","latin","lazy","legal","level","light","little","live","local","long","loud","low","loyal","lucky","lunar","mad","magic","main","major","marine","mass","maximum","mean","mere","middle","minimum","minor","minute","mixed","mobile","mutual","naive","narrow","nasty","near","negative","neutral","next","nice","noble","normal","notable","novel","nuclear","obvious","odd","olympic","online","only","open","ordinary","original","other","outdoor","outer","own","patient","perfect","physical","pink","polar","popular","possible","present","pretty","previous","primary","private","proud","public","pur","quick","quiet","random","rapid","rare","raw","ready","real","regular","rich","right","rigid","robust","rough","round","royal","rude","rural","sad","safe","same","senior","shallow","short","shy","sick","silent","silly","silver","similar","simple","slender","slight","slim","slow","small","smart","smooth","social","soft","solar","solid","sorry","south","spare","spatial","special","stable","still","strong","such","sudden","sunny","super","supreme","sure","sweet","swift","tall","that","then","there","they","this","tiny","tired","together","top","total","tragic","trim","true","truly","typical","ugly","unable","unaware","under","unfair","unhappy","uniform","unique","unknown","unusual","upper","urban","used","useful","useless","usual","vacant","vague","valid","various","vast","viable","vibrant","vicious","virtual","visual","vital","vivid","vocal","warm","way","weak","wealth","weird","welcome","well","west","wet","whole","wide","wild","wise","wrong","yellow","young"}

VERB_SET = {"abandon","absorb","abuse","achieve","acquire","act","adapt","add","adjust","admit","adopt","advance","afford","agree","aim","alert","allow","alter","amaze","announce","appear","approve","argue","arrange","arrive","ask","assist","assume","attack","attend","attract","avoid","balance","become","begin","believe","benefit","bind","blame","blast","bleed","bless","bloom","blow","blur","boil","boost","borrow","bounce","bring","brush","build","burst","call","cancel","capture","carry","catch","cause","change","charge","chase","check","choose","churn","circle","claim","clap","clarify","claw","climb","cling","clip","clutch","coach","coast","coil","collect","combine","come","confirm","connect","consider","control","convince","cook","copy","cover","crack","craft","crash","crawl","credit","crop","cross","crouch","crowd","crush","cry","curl","cushion","cycle","damage","dance","dash","deal","debate","decide","decline","decrease","define","defy","delay","deliver","demand","deny","depart","depend","derive","describe","design","despair","destroy","detect","develop","devote","differ","discover","dismiss","display","distance","divert","divide","document","donate","draft","draw","dress","drift","drill","drink","drip","drive","drop","earn","employ","enforce","engage","enhance","enjoy","enlist","enrich","enroll","ensure","enter","equip","erase","erode","evolve","exchange","excite","exclude","excuse","execute","exercise","exhibit","exist","exit","expand","expect","expire","explain","expose","express","extend","face","fade","fall","fan","fashion","fault","feature","feed","feel","fetch","film","filter","find","finish","fire","fit","flag","flash","flee","flip","float","flock","flush","fly","foam","focus","fold","follow","force","forget","foster","found","frown","fuel","furnace","fury","gather","gauge","gaze","giggle","give","glance","glare","glide","glimpse","glow","grab","grant","grasp","grow","grunt","guard","guess","guide","harvest","hire","hold","hope","host","hover","hunt","hurdle","hurry","identify","ignore","imitate","impose","improve","include","increase","indicate","inflict","inform","inhale","inject","install","invest","involve","isolate","join","judge","keep","kick","know","laugh","launch","learn","leave","lend","lift","link","list","load","loan","lock","manage","march","match","measure","melt","mention","merge","mix","model","modify","monitor","move","multiply","must","need","note","notice","obey","oblige","observe","obtain","occur","offer","omit","open","oppose","orbit","order","own","park","pass","patrol","pause","pave","permit","place","play","pledge","pluck","plug","plunge","ponder","predict","prefer","prepare","present","prevent","process","produce","program","project","promote","protect","provide","pull","punch","purchase","push","put","puzzle","quote","race","raise","rally","range","rate","reach","rebel","rebuild","recall","receive","recycle","reduce","reflect","reform","regret","reject","relax","release","rely","remain","remember","remind","remove","render","renew","rent","reopen","repair","repeat","replace","report","require","rescue","resemble","resist","retire","retreat","return","reveal","review","reward","ride","ring","ripple","risk","rival","roast","rotate","route","rule","run","saddle","sail","salute","satisfy","save","say","scan","scare","scatter","scout","screen","scrub","search","seat","section","seed","seek","segment","select","sell","sense","sentence","settle","setup","shadow","share","shed","shift","shine","shiver","shock","shoot","shop","shoulder","shove","shrug","shuffle","sign","sing","situate","skate","ski","slam","sleep","slice","slide","smile","smoke","snap","sniff","solve","sort","sound","source","spawn","speak","spell","spend","spin","split","sponsor","spot","spray","spread","spring","spy","squeeze","stamp","stand","start","state","stay","stem","step","stick","sting","stock","strike","struggle","stumble","submit","succeed","suffer","suggest","suit","supply","surge","surprise","surround","survey","suspect","sustain","swallow","swap","swarm","swear","swim","swing","switch","tackle","tag","talk","tape","target","task","taste","teach","tell","test","thank","thrive","throw","tilt","tip","topple","torch","toss","track","trade","train","transfer","trap","travel","treat","trend","trick","trigger","trim","trip","trouble","trust","try","tunnel","turn","twist","type","uncover","undo","unfold","unlock","update","upgrade","uphold","use","vanish","venture","verify","view","visit","voice","vote","wage","wait","walk","want","wash","waste","wave","wear","welcome","whip","whisper","win","wink","wish","witness","wonder","work","worry","wrap","wreck","wrestle","write"}

ADJ_SUFFIXES = ("able", "ible", "ful", "less", "ous", "ive", "al", "ial", "ic", "ical", "ant", "ent", "ary", "ory", "id", "ute", "ite")
VERB_SUFFIXES = ("ify", "ize", "ise", "ate", "ect", "ude", "mit")


def parse_words(path):
    words = []
    with open(path) as f:
        for line in f:
            m = re.search(r'"([a-z]+)"', line)
            if m:
                words.append(m.group(1))
    return words


def categorize(word):
    # Explicit sets checked first (adj before verb since some overlap like "open","welcome","trim","fit")
    if word in ADJ_SET:
        return 1
    if word in VERB_SET:
        return 2
    # Suffix heuristics
    if word.endswith(ADJ_SUFFIXES):
        return 1
    if word.endswith(VERB_SUFFIXES):
        return 2
    return 0  # noun


def main():
    words = parse_words(WORDLIST_H)
    assert len(words) == 2048, f"Expected 2048 words, got {len(words)}"

    cats = [categorize(w) for w in words]

    nouns = [i for i, c in enumerate(cats) if c == 0]
    adjs  = [i for i, c in enumerate(cats) if c == 1]
    verbs = [i for i, c in enumerate(cats) if c == 2]

    with open(OUTPUT_H, "w") as f:
        f.write("#ifndef WORDCAT_H\n#define WORDCAT_H\n#include <stdint.h>\n\n")
        f.write("/* 0=noun, 1=adjective, 2=verb */\n")

        # word_cat array
        f.write("static const uint8_t word_cat[2048] = {\n")
        for i in range(0, 2048, 16):
            chunk = cats[i:i+16]
            f.write("    " + ",".join(str(c) for c in chunk))
            if i + 16 < 2048:
                f.write(",")
            f.write("\n")
        f.write("};\n\n")

        # Index arrays
        def write_idx_array(name, indices):
            f.write(f"static const uint16_t {name}[] = {{\n")
            for i in range(0, len(indices), 16):
                chunk = indices[i:i+16]
                f.write("    " + ",".join(str(x) for x in chunk))
                if i + 16 < len(indices):
                    f.write(",")
                f.write("\n")
            f.write("};\n")

        write_idx_array("cat_nouns", nouns)
        f.write("\n")
        write_idx_array("cat_adjs", adjs)
        f.write("\n")
        write_idx_array("cat_verbs", verbs)

        f.write(f"\n#define CAT_NOUNS_LEN (sizeof(cat_nouns)/sizeof(cat_nouns[0]))\n")
        f.write(f"#define CAT_ADJS_LEN (sizeof(cat_adjs)/sizeof(cat_adjs[0]))\n")
        f.write(f"#define CAT_VERBS_LEN (sizeof(cat_verbs)/sizeof(cat_verbs[0]))\n")
        f.write("\n#endif\n")

    print(f"Generated {OUTPUT_H}")
    print(f"  Nouns:      {len(nouns)}")
    print(f"  Adjectives: {len(adjs)}")
    print(f"  Verbs:      {len(verbs)}")
    print(f"  Total:      {len(nouns)+len(adjs)+len(verbs)}")


if __name__ == "__main__":
    main()
