from flask import Flask, redirect, url_for, render_template, request
app = Flask(__name__)

states = ["NE", "AR", "AZ"]

@app.route('/')
def home():
    return redirect(url_for("Index"))

@app.route('/home', methods=["POST", "GET"])
def Index():
    if request.method == "POST":
        person = [request.form["first name"], request.form["last name"]]
        email = request.form["email"]
        address = [request.form["city"], request.form["state"], request.form["zip"]]
        tos = request.form["tos"]
        return redirect(url_for("mail"))
    else:
        return render_template("index.html", state_list=states)

@app.route("/mail")
def mail():
    return render_template("mail.html")

@app.route('/login', methods=["POST", "GET"])
def login():
    if request.method == "POST":
        person = request.form["email"]
        password = request.form["password"]
        return redirect(url_for("confirm", user=person))
    else:
        return render_template("login.html")

@app.route('/<usr>')
def user(usr):
    return f"<h1>{usr}</h1>"

@app.route("/news")
def news():
    return render_template("news.html")

@app.route('/info')
def info():
    return render_template("info.html")

@app.route('/problems')
def bugs():
    if request.method == "POST":
        bug = request.form["bug"]
        email = request.form["email"]
        return redirect(url_for("bugconfirm"))
    else:
        return render_template("bugs.html")

@app.route('/issueconfirm', methods=["POST"])
def issueconfirm():
    return render_template("issueconfirm.html")

@app.route('/bugconfirm', methods=["POST"])
def bugconfirm():
    return render_template("bugconfirm.html")

@app.route('/questions')
def questions():
    return render_template('/questions.html')

@app.route('/issues')
def issues():
    if request.method == "POST":
        email = request.form["email"]
        grievance = request.form["grievance"]
        return redirect(url_for("issueconfirm"))
    else:
        return render_template('/issues.html')

@app.route('/login/confirm/<user>', methods=["GET"])
def confirm(user):
    return render_template("confirm.html", person=user)


if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0')

