# WebRTC

WebRTC is a free, open project that provides browsers and mobile applications with
Real-Time Communications (RTC) capabilities via simple APIs. It's supported by most
major browsers (except Safari).

## WebRTC APIs
To acquire and communicate streaming data, WebRTC implements the following APIs:

* MediaStream: get access to data streams, such as from the user's camera and microphone.
* RTCPeerConnection: audio or video calling, with facilities for encryption and bandwidth management.
* RTCDataChannel: peer-to-peer communication of generic data.

In other words, WebRTC hides the internal details of capturing and sharing the video and audio streams
through a P2P connection.

## External servers

WebRTC still needs servers:

* For clients to exchange metadata to coordinate communication: this is called **signaling**.
* To cope with network address translators (NATs) and firewalls. This is called Interactive
Connectivity Establishment (**ICE**)

### Signaling

In order for a WebRTC application to set up a 'call', its clients need to exchange information:

* Session control messages used to open or close communication.
* Media metadata such as codecs and codec settings, bandwidth and media types.
* Network data, such as a host's IP address and port as seen by the outside world.

This signaling process needs a way for clients to pass messages back and forth. That mechanism is not implemented by the WebRTC APIs: you need to build it yourself.

Example: https://github.com/lambdaclass/webrtc-server/blob/63444bdbe8d59fb5c2291383b4cfb58fef725bdd/src/ws_handler.erl

### ICE

ICE tries to find the best path to connect peers. It tries all possibilities in parallel and
chooses the most efficient option that works.

* Try to make a connection using the host address obtained from a device's operating system and network card.
* If that fails (which it will for devices behind NATs) ICE obtains an external address using a STUN server.
* If that fails, traffic is routed via a TURN relay server.

https://www.html5rocks.com/en/tutorials/webrtc/infrastructure/turn.png

## Client example
```js
var signalingChannel = createSignalingChannel();
var pc;
var configuration = ...;

// run start(true) to initiate a call
function start(isCaller) {
    pc = new RTCPeerConnection(configuration);

    // send any ice candidates to the other peer
    pc.onicecandidate = function (evt) {
        signalingChannel.send(JSON.stringify({ "candidate": evt.candidate }));
    };

    // once remote stream arrives, show it in the remote video element
    pc.onaddstream = function (evt) {
        remoteView.src = URL.createObjectURL(evt.stream);
    };

    // get the local stream, show it in the local video element and send it
    navigator.getUserMedia({ "audio": true, "video": true }, function (stream) {
        selfView.src = URL.createObjectURL(stream);
        pc.addStream(stream);

        if (isCaller)
            pc.createOffer(gotDescription);
        else
            pc.createAnswer(pc.remoteDescription, gotDescription);

        function gotDescription(desc) {
            pc.setLocalDescription(desc);
            signalingChannel.send(JSON.stringify({ "sdp": desc }));
        }
    });
}

signalingChannel.onmessage = function (evt) {
    if (!pc)
        start(false);

    var signal = JSON.parse(evt.data);
    if (signal.sdp)
        pc.setRemoteDescription(new RTCSessionDescription(signal.sdp));
    else
        pc.addIceCandidate(new RTCIceCandidate(signal.candidate));
};
```
