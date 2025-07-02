from flask import Flask, send_from_directory, jsonify
from flask_cors import CORS
import os
from parse_smed import parse_smed_file

app = Flask(__name__, static_folder="build", static_url_path="/")
CORS(app)

@app.route("/")
def serve_react():
    return send_from_directory(app.static_folder, "index.html")

@app.route("/api/smed")
def get_smed():
    path = os.path.join(os.getcwd(), "public", "SMED_FILES", "LOGO.smed")
    return jsonify(parse_smed_file(path)) 

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8000)

