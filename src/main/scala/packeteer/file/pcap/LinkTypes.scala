package packeteer.file.pcap

import packeteer.util.Unsigned



final case class LinkType(toInt : Int) extends AnyVal with Unsigned.IntWrapper


/**
 * PCAP link types as specified by http://www.tcpdump.org/linktypes.html
 */
object LinkTypes
{
	val LINKTYPE_NULL = 
		LinkType(0)
	val LINKTYPE_ETHERNET = 
		LinkType(1)
	val LINKTYPE_AX25 = 
		LinkType(3)
	val LINKTYPE_IEEE802_5 = 
		LinkType(6)
	val LINKTYPE_ARCNET_BSD = 
		LinkType(7)
	val LINKTYPE_SLIP = 
		LinkType(8)
	val LINKTYPE_PPP = 
		LinkType(9)
	val LINKTYPE_FDDI = 
		LinkType(10)
	val LINKTYPE_PPP_HDLC = 
		LinkType(50)
	val LINKTYPE_PPP_ETHER = 
		LinkType(51)
	val LINKTYPE_ATM_RFC1483 = 
		LinkType(100)
	val LINKTYPE_RAW = 
		LinkType(101)
	val LINKTYPE_C_HDLC = 
		LinkType(104)
	val LINKTYPE_IEEE802_11 = 
		LinkType(105)
	val LINKTYPE_FRELAY = 
		LinkType(107)
	val LINKTYPE_LOOP = 
		LinkType(108)
	val LINKTYPE_LINUX_SLL = 
		LinkType(113)
	val LINKTYPE_LTALK = 
		LinkType(114)
	val LINKTYPE_PFLOG = 
		LinkType(117)
	val LINKTYPE_IEEE802_11_PRISM = 
		LinkType(119)
	val LINKTYPE_IP_OVER_FC =
		LinkType(112)
	val LINKTYPE_SUNATM =
		LinkType(123)
	val LINKTYPE_IEEE802_11_RADIOTAP = 
		LinkType(127)
	val LINKTYPE_ARCNET_LINUX =
		LinkType(129)
	val LINKTYPE_APPLE_IP_OVER_IEEE1394 =
		LinkType(138)
	val LINKTYPE_MTP2_WITH_PHDR =
		LinkType(139)
	val LINKTYPE_MTP2 =
		LinkType(140)
	val LINKTYPE_MTP3 =
		LinkType(141)
	val LINKTYPE_SCCP =
		LinkType(142)
	val LINKTYPE_DOCSIS = 
		LinkType(143)
	val LINKTYPE_LINUX_IRDA =
		LinkType(144)
	val LINKTYPE_USER = for (n <- 147 to 162) yield LinkType(n)
	val LINKTYPE_IEEE802_11_AVS =
		LinkType(163)
	val LINKTYPE_BACNET_MS_TP = 
		LinkType(165)
	val LINKTYPE_PPP_PPPD =
		LinkType(166)
	val LINKTYPE_GPRS_LLC =
		LinkType(169)
	val LINKTYPE_LINUX_LAPD =
		LinkType(177)
	val LINKTYPE_BLUETOOTH_HCI_H4 =
		LinkType(187)
	val LINKTYPE_USB_LINUX =
		LinkType(189)
	val LINKTYPE_PPI =
		LinkType(192)
	val LINKTYPE_IEEE802_15_4 =
		LinkType(195)
	val LINKTYPE_SITA =
		LinkType(196)
	val LINKTYPE_ERF =
		LinkType(197)
	val LINKTYPE_BLUETOOTH_HCI_H4_WITH_PHDR =
		LinkType(201)
	val LINKTYPE_AX25_KISS = 
		LinkType(202)
	val LINKTYPE_LAPD =
		LinkType(203)
	val LINKTYPE_PPP_WITH_DIR =
		LinkType(204)
	val LINKTYPE_C_HDLC_WITH_DIR =
		LinkType(205)
	val LINKTYPE_FRELAY_WITH_DIR =
		LinkType(206)
	val LINKTYPE_IPMB_LINUX =
		LinkType(209)
	val LINKTYPE_IEEE802_15_4_NONASK_PHY =
		LinkType(215)
	val LINKTYPE_USB_LINUX_MMAPPED =
		LinkType(220)
	val LINKTYPE_FC_2 =
		LinkType(224)
	val LINKTYPE_FC_2_WITH_FRAME_DELIMS =
		LinkType(225)
	val LINKTYPE_IPNET =
		LinkType(226)
	val LINKTYPE_CAN_SOCKETCAN =
		LinkType(227)
	val LINKTYPE_IPV4 =
		LinkType(228)
	val LINKTYPE_IPV6 =
		LinkType(229)
	val LINKTYPE_IEEE802_15_4_NOFCS =
		LinkType(230)
	val LINKTYPE_DBUS =
		LinkType(231)
	val LINKTYPE_DVB_CI =
		LinkType(235)
	val LINKTYPE_MUX27010 =
		LinkType(236)
	val LINKTYPE_STANAG_5066_D_PDU =
		LinkType(237)
	val LINKTYPE_NFLOG =
		LinkType(239)
	val LINKTYPE_NETANALYZER =
		LinkType(240)
	val LINKTYPE_NETANALYZER_TRANSPARENT =
		LinkType(241)
	val LINKTYPE_IPOIB =
		LinkType(242)
	val LINKTYPE_MPEG_2_TS =
		LinkType(243)
	val LINKTYPE_NG40 =
		LinkType(244)
	val LINKTYPE_NFC_LLCP =
		LinkType(245)
	val LINKTYPE_INFINIBAND =
		LinkType(247)
	val LINKTYPE_SCTP =
		LinkType(248)
	val LINKTYPE_USBPCAP =
		LinkType(249)
	val LINKTYPE_RTAC_SERIAL =
		LinkType(250)
	val LINKTYPE_BLUETOOTH_LE_LL =
		LinkType(251)
}