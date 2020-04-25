stack-all:
	stack --resolver nightly build
	@echo
	stack --resolver lts-15 build
	@echo
	stack --resolver lts-14 build
	@echo
	stack --resolver lts-13 build
	@echo
	stack --resolver lts-12 build
	@echo
	stack --resolver lts-11 --stack-yaml stack-lts11.yaml build
	@echo
	stack --resolver lts-10 --stack-yaml stack-lts11.yaml build
#	@echo
#	stack --resolver lts-9 build
#	@echo
#	stack --resolver lts-8 build
#	@echo
#	stack --resolver lts-6 build
