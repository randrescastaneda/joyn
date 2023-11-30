.joynenv <-  new.env(parent = emptyenv())
rlang::env_poke(.joynenv, "msg_type_choices", c("info", "note", "warn", "timing", "err"))
