# thrift-cli

## inspiration

This is a cmd tool which will build the request payload to thrift value and then send the message to the server and receive the response from the service.

This tool is just like the `curl` tool.



## cmd param


```
thrift-cli  {protocol}//{ip}:{host}/{serviceName}/{functionName} -d '{payload}'  --file {filepath}   -t {payloadType}
```


## example


```
thrift-cli  binary//127.0.0.1:9091/DemoService/demoFunction -d '{"msg1":{"msg":"m1","xx":"x1"},"msg2":{"msg":"m2","xx":"x2"}}'  --file "IDL/service/demo.thrift"   -t json
```



## TODO
- [ ] thrift transport protocol
- [ ] payload type
- [ ] dir param . use the `serviceName` and `functionName` to find the thrift file.
