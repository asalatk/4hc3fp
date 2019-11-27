# Running:

* Run ```elm reactor```
* Open ```index.html```

In case the Elm Application does not render, run ```./make.sh``` which ensures that the dependencies get installed.

# Developing:

Boot time!

Steps:
* Modify ```src/Main.elm```. Currently this contains code which gets rendered as bootstrap.
* Once done making changes, run ```make.sh```, which compiles the ```Main.elm``` into ```main.js``` that is used in ```index.html``` to render the compiled code.
* Use ```elm reactor``` and open up ```index.html```.
* Continue making changess 


# Building:

Just run ```make.sh``` when you're done making changes in ```Main.elm```

