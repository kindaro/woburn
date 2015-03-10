dots = $(wildcard *.dot)
svgs = $(patsubst %.dot, %.svg, $(dots))

all: $(svgs)

%.svg : %.dot
	dot -Tsvg -o $@ $<
