# Introduction #

MQTT is a lightweight message passing protocol.


# Configuring zmqtt #


mod\_mqtt -> start\_broker -> true
mod\_mqtt -> port -> some\_value
mod\_mqtt -> allow\_anonymous -> some\_boolean
mod\_mqtt -> username -> some\_value
mod\_mqtt -> password -> some\_value

When a client is connecting to a broker from the same same site, it can use the site's config variables for username and password, so the call is simply connect(Context).

! The data in the packets including the username and password are not encrypted.