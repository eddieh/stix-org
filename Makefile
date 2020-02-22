.PHONY: all build clean

all: build

index.org: example.org
	cp example.org index.org

docs:
	mkdir -p docs

docs/index.html: init.el docs index.org
	emacs index.org -q --batch -l init.el -f example-org-html-export-to-github-pages
	mv index.html docs

build: docs/index.html
	cp -r css docs
	cp -r img docs

clean:
	rm index.html index.org
