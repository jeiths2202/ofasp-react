def parse_smed_file(path):
    fields = []

    with open(path, encoding='shift_jis') as f:
        for line in f:
            line = line.strip()
            if not line.startswith("ITEM"):
                continue  # ITEM ?? ??? skip

            parts = line.split()
            item = {
                "name": parts[1],
                "type": "A",
                "position": { "row": 0, "col": 0 },
                "length": 0,
                "prompt": "",
                "color": "#4ADE80"
            }

            for p in parts[2:]:
                if p.startswith("POS="):
                    m = p[4:].strip("()").split(",")
                    item["position"]["row"] = int(m[0])
                    item["position"]["col"] = int(m[1])
                elif p.startswith("LEN="):
                    item["length"] = int(p[4:])
                elif p.startswith("PROMPT="):
                    prompt_match = line.split('PROMPT="')
                    if len(prompt_match) > 1:
                        item["prompt"] = prompt_match[1].split('"')[0]
                elif p.startswith("COLOR="):
                    item["color"] = p.split("=")[1]
                elif p.startswith("TYPE="):
                    item["type"] = p.split("=")[1]

            fields.append(item)

    return { "fields": fields }

