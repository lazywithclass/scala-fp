start:
	docker run -i -v `pwd`/jdegoes-functional-scala:/workspace -t scala-fp

build: 
	docker build -t scala-fp .

clean:
	docker stop `docker ps -a -q`
	docker rm `docker ps -a -q`
	docker rmi `docker images -q`
