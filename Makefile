jsdir = app/assets/javascripts
cssdir = app/assets/stylesheets
jsvendordir = vendor/assets/javascripts
jslibdir = vendor/assets/javascripts
rootjsfiles := $(wildcard $(jsdir)/*.js)
jslibfiles := $(jsvendordir)/jquery.js\
	      $(jsvendordir)/jquery_ujs.js\
	      $(jsvendordir)/date.js\
	      $(jsvendordir)/highcharts.js\
	      $(jsvendordir)/d3.js\
	      $(jsvendordir)/jquery.tokeninput.js\
	      $(jsvendordir)/jquery.fancybox.js\
	      $(jsvendordir)/underscore.js\
	      $(jsvendordir)/backbone.js\
	      $(jsvendordir)/backbone.localStorage.js\
	      $(jsvendordir)/scramble_pyraminx.js\
	      $(jsvendordir)/scramble_2x2x2.js
jsfiles := $(jsdir)/cubemania.js\
	   $(wildcard $(jsdir)/models/*.js)\
	   $(wildcard $(jsdir)/collections/*.js)\
	   $(wildcard $(jsdir)/views/**/*.js)\
	   $(wildcard $(jsdir)/presenters/*.js)\
	   $(filter-out $(jsdir)/cubemania.js $(jsdir)/init.js, $(rootjsfiles))\
	   $(jsdir)/init.js
cssfiles := $(wildcard $(cssdir)/*.scss)\
	    $(wildcard $(cssdir)/**/*.scss)\
	    vendor/assets/stylesheets/jquery.fancybox.css.scss

public/assets/app.js: $(jsfiles) $(jslibfiles)
	@mkdir -p public/assets
	cat $(jslibfiles) $(jsfiles) > $@

public/assets/app.css: $(cssfiles)
	sass vendor/assets/stylesheets/jquery.fancybox.css.scss > $@
	sass app/assets/stylesheets/application.scss >> $@

public/assets/app.min.js: public/assets/app.js
	uglifyjs $^ > $@

public/assets/images: app/assets/images vendor/assets/images
	cp -r app/assets/images $@
	cp vendor/assets/images/* $@

api/dist/api-exe: api/src/*.hs api/app/Main.hs
	cd api; mkdir -p dist; stack build

assets: public/assets/app.min.js public/assets/app.css public/assets/images

clean:
	rm -r public/assets
	rm -r api/dist

all: assets api/dist/api-exe

.PHONY: assets clean all
