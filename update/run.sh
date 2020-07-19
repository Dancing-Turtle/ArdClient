#!/bin/bash
java -Djogl.disable.opengles=true -Xss1024k -Xms512m -Xmx1024m -jar build/hafen.jar -U https://game.havenandhearth.com/hres/ game.havenandhearth.com
