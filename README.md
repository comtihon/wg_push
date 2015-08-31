# wg_push

Erlang library for working with
[Apple Push Notification Service](https://developer.apple.com/library/mac/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/ApplePushService.html)

See [test/wg_apns_push_SUITE.erl](test/wg_apns_push_SUITE.erl) for usage sample.


#### Start connection:
Minimal:

	wg_push:start(#{name => my_registered_name, host => "host", port => 2195, certfile => "certfile path", keyfile => "keyfile"}).
Also, callbacks for errors can be added:

	#{on_retry => fun(Messages, Position, Reason) -> RetryMessages end}.
Which allows to resend messages, which are returned by callback, if error occurred.

	#{on_error => fun(Error) -> log(Error) end}
	
Which allows to log error.