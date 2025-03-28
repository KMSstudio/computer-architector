buildbase = ../build
bdir = $(buildbase)/bdir
bindir = $(buildbase)/bin
simdir = $(buildbase)/simdir
libbase = ../lib

compile:
	mkdir -p $(simdir)
	mkdir -p $(bdir)
	mkdir -p $(bindir)

fold: compile
	rm -rf $(bdir)/mkTbFftFolded* $(simdir)/* $(bindir)/sim_fold.so $(bindir)/sim_fold $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -D Folded -bdir $(bdir) -simdir $(simdir) -vdir $(buildbase) -p $(libbase):+ $(libbase)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -D Folded -bdir $(bdir) -simdir $(simdir) -p $(libbase):+ -o $(bindir)/sim_fold -e mkTbFftFolded $(bdir)/*.ba $(libbase)/*.cpp

pipe: compile
	rm -rf $(bdir)/mkTbFftPipelined* $(simdir)/* $(bindir)/sim_pipe.so $(bindir)/sim_pipe $(libbase)/*.o
	bsc -u -sim -aggressive-conditions -D Pipelined -bdir $(bdir) -simdir $(simdir) -vdir $(buildbase) -p $(libbase):+ $(libbase)/TestBench.bsv
	bsc -u -sim -aggressive-conditions -D Pipelined -e mkTbFftPipelined -bdir $(bdir) -simdir $(simdir) -p $(libbase):+ -o $(bindir)/sim_pipe -e mkTbFftPipelined $(bdir)/*.ba $(libbase)/*.cpp

clean:
	rm -rf $(buildbase)

.PHONY: clean fold pipe
.DEFAULT_GOAL := fold
