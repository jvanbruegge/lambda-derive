all:
	@mkdir -p build
	@rm -rf ./build/*
	@stack clean --docker
	@stack build --docker
	@cp `stack --docker path --local-install-root`/bin/bootstrap build
	@cd build && zip function.zip bootstrap && rm bootstrap && cd ..

deploy:
	@cd terraform && terraform apply -var-file="vars.tfvars"

destroy:
	@cd terraform && terraform destroy -var-file="vars.tfvars"
