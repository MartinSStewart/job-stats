module Data exposing (data)

import Set exposing (Set)


{-| All the messages that I considered to be job postings
-}
data : Set Int
data =
    Set.fromList
        [ 1664480073036
        , 1664275467484
        , 1663746206257
        , 1661926141556
        , 1660811800573
        , 1652388610065
        , 1652300880819
        , 1652236800286
        , 1652211686225
        , 1652211686225
        , 1652202970142
        , 1651518050371
        , 1651505853118
        , 1649696930879
        , 1649425390140
        , 1648565833583
        , 1647983203403
        , 1647530327361
        , 1647424482589
        , 1647009471283
        , 1646950841851
        , 1646651147199
        , 1646425559063
        , 1646236799503
        , 1645729012561
        , 1645713559033
        , 1645670488637
        , 1645036233448
        , 1644918555968
        , 1644873557834
        , 1644873557834
        , 1644594793222
        , 1644536421084
        , 1644418817122
        , 1643660416526
        , 1643595399735
        , 1643138973067
        , 1643129382063
        , 1643021670040
        , 1642981231038
        , 1642612648035
        , 1642113182031
        , 1641991513026
        , 1641975234022
        , 1641926614018
        , 1641459340004
        , 1641233089177
        , 1640032258161
        , 1639405335154
        , 1638834635124
        , 1638802991119
        , 1638299365115
        , 1638252426113
        , 1638201036111
        , 1637955341105
        , 1637663540103
        , 1637568887098
        , 1637398837096
        , 1636594159087
        , 1636585758085
        , 1635955165067
        , 1635780952063
        , 1635418221024
        , 1635270178016
        , 1635270178016
        , 1635148067006
        , 1633083055125
        , 1632870124116
        , 1632866241111
        , 1632783862096
        , 1632738353094
        , 1632260644075
        , 1632222444072
        , 1632211071065
        , 1631835816057
        , 1631082328028
        , 1630679101015
        , 1630589327011
        , 1630324411003
        , 1629873961058
        , 1629714729039
        , 1628814305024
        , 1628698024018
        , 1628271475017
        , 1628174716016
        , 1627674571005
        , 1627591102148
        , 1627470464143
        , 1627305560136
        , 1627012306132
        , 1626571758128
        , 1626276725123
        , 1626198344112
        , 1625639372104
        , 1625144718097
        , 1624892218089
        , 1624735311079
        , 1624368712066
        , 1624300842057
        , 1624300842057
        , 1623418413047
        , 1623261254026
        , 1623163335023
        , 1621957662015
        , 1621530563007
        , 1621436273006
        , 1620311299162
        , 1620292915156
        , 1619798367151
        , 1619511491142
        , 1619451849136
        , 1619210524125
        , 1619020348117
        , 1619016743116
        , 1619005877115
        , 1618954553104
        , 1618902274101
        , 1618840408099
        , 1618840408099
        , 1618836645098
        , 1618229748082
        , 1617731649072
        , 1617385092056
        , 1617333455038
        , 1617292565036
        , 1616679894023
        , 1616593259021
        , 1616008776008
        , 1615991180006
        , 1615218173052
        , 1614851302048
        , 1614629727040
        , 1614393066021
        , 1614328055017
        , 1614169375013
        , 1613964606004
        , 1613132151040
        , 1612944735032
        , 1612868661030
        , 1612867501024
        , 1612826107013
        , 1611784659006
        , 1611730905003
        , 1608309896266
        , 1608067762255
        , 1608045154249
        , 1607693307244
        , 1607520320235
        , 1606861756214
        , 1606816885210
        , 1606484255204
        , 1605798101192
        , 1605545568184
        , 1605543522180
        , 1605371001141
        , 1604442498137
        , 1604442498137
        , 1604000401133
        , 1603780162125
        , 1603354574120
        , 1602849202099
        , 1602254603089
        , 1602254564089
        , 1602237120088
        , 1602233565087
        , 1602164402083
        , 1602159010078
        , 1602033830073
        , 1601982084059
        , 1601540537050
        , 1601405203036
        , 1601393634024
        , 1601382440016
        , 1600348074020
        , 1600348074020
        , 1599086256009
        , 1598630818016
        , 1597395960256
        , 1597060803248
        , 1596845323228
        , 1596820386219
        , 1596744510214
        , 1595679333192
        , 1595448436180
        , 1594192058164
        , 1594056382155
        , 1593604487133
        , 1591290369098
        , 1591290369098
        , 1591121210094
        , 1590596760088
        , 1588821858049
        , 1587993207039
        , 1584992325075
        , 1583477241046
        , 1583082214035
        , 1581967989024
        , 1580836091017
        , 1579817974006
        , 1579802603001
        , 1579599270007
        , 1579106216008
        , 1577938160006
        , 1574092697050
        , 1573643294027
        , 1572545695002
        , 1572041481037
        , 1571841459032
        , 1571761424027
        , 1571416150014
        , 1571244942005
        , 1570478957033
        , 1569782435024
        , 1569678851021
        , 1569678851021
        , 1569529969020
        , 1569404227017
        , 1568809259010
        , 1568395988032
        , 1568360827015
        , 1566979560002
        , 1565806552040
        , 1565796823040
        , 1565715236039
        , 1565259536026
        , 1565175340024
        , 1564783950019
        , 1564179318011
        , 1564179318011
        , 1564001881006
        , 1563978114005
        , 1563871145002
        , 1563455744085
        , 1563448377083
        , 1562940684067
        , 1562840981065
        , 1562801367062
        , 1561924103055
        , 1561557638050
        , 1561391812048
        , 1561108772042
        , 1560886722040
        , 1560885055039
        , 1560415846015
        , 1560370990012
        , 1560367096012
        , 1560321033010
        , 1559126022004
        , 1559126022004
        , 1558529526084
        , 1558099493066
        , 1557919436057
        , 1557813155036
        , 1557774528033
        , 1557357816017
        , 1556089094035
        , 1555491444031
        , 1555491157028
        , 1555149189004
        , 1555084076000
        , 1554807616010
        , 1554802358006
        , 1554391890002
        , 1554391890002
        , 1553177591031
        , 1552477665017
        , 1551981540007
        , 1551975228006
        , 1551369153002
        , 1551193094020
        , 1551099179018
        , 1551031729014
        , 1550763391004
        , 1550512371008
        , 1549362759073
        , 1546869313010
        , 1545330993018
        , 1544680753010
        , 1544641700007
        , 1544641700007
        , 1543364393034
        , 1542624719029
        , 1542370315026
        , 1542203129021
        , 1542131897018
        , 1542055479013
        , 1541462674008
        , 1539243458000
        , 1538594492000
        , 1538485668000
        , 1538410265000
        , 1538410265000
        , 1538166838000
        , 1538159915000
        , 1537797188000
        , 1537539340000
        , 1536612361000
        , 1535450824000
        , 1535440721000
        , 1535132772000
        , 1535002202000
        , 1534976849000
        , 1533793180000
        , 1531939581000
        , 1531252697001
        , 1530813904000
        , 1530813904000
        , 1530780742000
        , 1530591042000
        , 1528905080001
        , 1528791257000
        , 1527603070001
        , 1527197191000
        , 1527040961000
        , 1526916078000
        , 1526482971000
        , 1526480548001
        , 1525340404000
        , 1524629118000
        , 1524137399000
        , 1522759937000
        , 1522351189001
        , 1522328254000
        , 1522071098000
        , 1521485379001
        , 1521039477000
        , 1520116969000
        , 1520018853000
        , 1520018401001
        , 1518604844000
        , 1516936185000
        , 1516272925000
        , 1515609384001
        , 1515048887000
        , 1514533601000
        , 1514149401000
        , 1513427884000
        , 1513159752000
        , 1511368301000
        , 1510089006000
        , 1510089006000
        , 1509644487001
        , 1509511188000
        , 1509480918000
        , 1509360272000
        , 1508801752000
        , 1508004454000
        , 1507870538000
        , 1507199818000
        , 1507134679000
        , 1507016362000
        , 1506963972000
        , 1506530957001
        , 1506294629000
        , 1505339220000
        , 1505250768001
        , 1505238953000
        , 1505229339000
        , 1505205027000
        , 1504707637001
        , 1504158195000
        , 1504099429000
        , 1504046545000
        , 1503303450000
        , 1503303450000
        , 1503120806000
        , 1502891700000
        , 1501273864997
        , 1501080143035
        , 1500387790775
        , 1499862286869
        , 1499108574231
        , 1498501413766
        , 1498501413766
        , 1498473963916
        , 1497950942476
        , 1497778920094
        , 1497548631310
        , 1497548123139
        , 1495664504999
        , 1494941039868
        , 1494940772793
        , 1493076748612
        , 1493076748612
        , 1492267323197
        , 1492162863721
        , 1492102435298
        , 1492034004671
        , 1491637082333
        , 1491569735199
        , 1491206280828
        , 1490213087439
        , 1490095520378
        , 1488979198000
        , 1487550688000
        , 1486473135000
        , 1484931518000
        , 1484752725000
        , 1483991759000
        , 1482056869000
        , 1482056869000
        , 1481219115000
        , 1481133336000
        , 1480949921000
        , 1480934055000
        , 1480934055000
        , 1480899592000
        , 1479993301000
        , 1479925147000
        , 1478112625000
        , 1476393420000
        , 1476112455000
        , 1475706067000
        , 1475531981000
        , 1473242174000
        , 1467155679000
        , 1461980961000
        , 1461773923000
        , 1461762367000
        , 1460578722000
        , 1456139303000
        , 1455207808000
        ]
