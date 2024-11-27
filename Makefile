TARFILE := $(wildcard Code*.tar.gz)
all:
	tar -xf $(TARFILE)
	mkdir -p plot/out
	cd plot && Rscript param-estimator.R
	cd plot && Rscript param-estimator-2d.R
	cd plot && Rscript permanogva.R
	cd plot && Rscript anogva.R
	cd plot && Rscript model-selection.R
	cd plot && Rscript k-medoids.R
	cd plot && Rscript search-comparison.R
	cd plot && Rscript time.R

pp:
	tar -xf $(TARFILE)
	mkdir -p plot2/out
	cd plot2 && Rscript param-estimator.R
	cd plot2 && Rscript param-estimator-2d.R
	cd plot2 && Rscript permanogva.R
	cd plot2 && Rscript anogva.R
	cd plot2 && Rscript model-selection.R
	cd plot2 && Rscript k-medoids.R
	cd plot2 && Rscript search-comparison.R
	cd plot2 && Rscript time.R

clean:  
	rm -rf Code
	rm -f plot/out/*
	rm -f plot2/out/*

