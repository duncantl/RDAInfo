# ReadItemInfo = []
import json

def write(x, filename = "/tmp/foo.json"):
    fp = open(filename, "w")
    fp.write(json.dumps(x))
    fp.close()
    filename
