from callbacks import *
from layouts import main_layout

app.layout = main_layout

if __name__ == "__main__":
    app.run_server(debug=True)
