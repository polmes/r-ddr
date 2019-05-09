DATAFILES:=$(wildcard ../data/*.so6)
PROCESSEDFILES=$(DATAFILES:=.pre)


preprocess: pre.cpp
	@echo $(DATAFILES)
	@echo $(PROCESSEDFILES)
	g++ -Ofast $< -o $@


../data/%.so6.pre: ../data/%.so6 preprocess
	./preprocess $< > $@

predata: $(PROCESSEDFILES)
