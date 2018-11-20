#!/usr/bin/python3
"""
Konsolowa aplikacja mająca pomóc skonfigurować OpenVPN dla servera i klienta. Forked from Angristan/OpenVPN-install. Thanks!
"""

import sys
import os
import shutil
import re
import random
from os import environ as ENV

# Global variable
OperationSystem = ""
version_id = ""  # for debain versions
# File to iptables
IPTABLES = ""
# Configuration file for setting system variables
SYSCTL = ""

if ENV["USER"] != 'root':
    print("[!]This script must be run as root.")
    sys.exit(0)

if not os.path.exists("/dev/net/tun"):
    print("[!]TUN device is not installed on this system")
    sys.exit(1)

if os.path.exists("/etc/debian_version"):
    OperationSystem = "debian"
    version_id = str(os.popen('cat /etc/os-release | grep "VERSION_ID"').read()).strip()
    IPTABLES = '/etc/iptables/iptables.rules'
    SYSCTL = '/etc/sysctl.conf'
    if not version_id in ['VERSION_ID="7"', 'VERSION_ID="8"', 'VERSION_ID="9"', 'VERSION_ID="14.04"',
                          'VERSION_ID="16.04"', 'VERSION_ID="17.10"', 'VERSION_ID="18.04"']:
        print("[!]Your version of Ubuntu/Debian is not supported")
        sys.exit(2)
elif os.path.exists("/etc/fedora-release"):
    OperationSystem = "fedora"
    IPTABLES = '/etc/iptables/iptables.rules'
    SYSCTL = '/etc/sysctl.d/openvpn.conf'
else:
    print("[!]Your system is not support by this setup script")
    sys.exit(2)


def add_client(client_name: str) -> str:
    '''
    Script for create <ClientName>.ovpn
    configuration file by sEASYRSA_PKIelected path
    :return: 0
    '''
    print("[>>>>] in add_client")
    if client_name == "":
        raise Exception("[!] Error client name")
    homeDir = ""
    if os.path.exists("/home/" + str(client_name).strip()):
        homeDir = "/home/" + str(client_name).strip()
    elif os.path.exists("/home"):
        homeDir = "/home"
    elif "HOME" in os.environ.keys():
        homeDir = ENV["HOME"]
    else:
        homeDir = "/root"
    destConfigFile = "{}/{}.ovpn".format(homeDir, client_name)
    print(destConfigFile)
    shutil.copy("/etc/openvpn/client-template.txt", destConfigFile)
    with open(destConfigFile, "a") as file:
        XMLAdd = lambda tag, path: "<{0}>\n{1}</{0}>\n".format(tag, os.popen("cat {}".format(path), mode="r").read())
        string = XMLAdd(tag='ca', path='/etc/openvpn/easy-rsa/pki/ca.crt')
        string += XMLAdd(tag='cert', path="/etc/openvpn/easy-rsa/pki/issued/{}.crt".format(client_name))
        string += XMLAdd(tag='key', path="/etc/openvpn/easy-rsa/pki/private/{}.key".format(client_name))
        string += "key-direction 1\n"
        string += XMLAdd(tag='tls-auth', path="/etc/openvpn/tls-auth.key")
        file.write(string)
    return destConfigFile


# Get default route connection interface
# Example: eth0, wlp2s0
NIC = os.popen("echo $(ip -4 route ls | grep default | grep -Po '(?<=dev )(\S+)' | head -1)", mode="r").read().strip()

if os.path.exists('/etc/openvpn/server.conf'):
    while(True):
        print("OpenVPN-install (github.com/Angristan/OpenVPN-install)")
        print("")
        print("Looks like OpenVPN is already installed")
        print("")
        print("What do you want to do?")
        print("   1) Add a cert for a new user")
        print("   2) Revoke existing user cert")
        #print("   3) Remove OpenVPN")
        print("   3) Exit")
        option = input("Select an option [1-3]: ")


        def main_option_for_new_user():
            print("[>>>>]in main_option_for_new_user")
            print("\nTell me a name for the client cert\nPlease, use one word only, no special characters")
            client_name = input("Client name: ").strip()
            os.chdir("/etc/openvpn/easy-rsa/")
            os.popen("/etc/openvpn/easy-rsa/easyrsa build-client-full {} nopass".format(client_name), mode='r').read()
            # Generates the custom client.ovpn
            path_file = add_client(client_name)
            print("\nClient {} added, certs available at {}".format(client_name, path_file))
            sys.exit(1)


        def main_option_revoke_user():
            os.chdir("/etc/openvpn/easy-rsa/")
            number_of_client = os.popen('echo $(tail -n +2 /etc/openvpn/easy-rsa/pki/index.txt | grep -c "^V")', mode="r").read().strip()
            if number_of_client == '0':
                print("\nYou have no existing clients\n")
                sys.exit(5)
            print("\nSelect the existing client certificate you want to revoke")
            print(os.popen("tail -n +2 /etc/openvpn/easy-rsa/pki/index.txt | grep \"^V\" | cut -d '=' -f 2 | nl -s ') '", mode='r').read())
            client_num = ''
            if number_of_client == '1':
                client_num = input("Select one client [1]: ")
            else:
                client_num = input("Select one client [1-{}]: ".format(number_of_client))
            client_name = str(os.popen('echo $(tail -n +2 /etc/openvpn/easy-rsa/pki/index.txt | grep "^V" | cut -d \'=\' -f 2 | sed -n "{}"p)'.format(
                    client_num), mode="r").read()).strip()
            os.popen('/etc/openvpn/easy-rsa/easyrsa --batch revoke {}'.format(client_name)).read()
            EASYRSA_CRL_DAYS = 3650
            os.popen('/etc/openvpn/easy-rsa/easyrsa gen-crl', mode="r").read()

            try:
                os.remove("/etc/openvpn/easy-rsa/pki/reqs/{}.req".format(client_name))
            except:
                print("file not found " + "/etc/openvpn/easy-rsa/pki/reqs/{}.req")

            try:
                os.remove("/etc/openvpn/easy-rsa/pki/private/{}.key".format(client_name))
            except:
                print("file not found")
            try:
                os.remove("/etc/openvpn/easy-rsa/pki/issued/{}.crt".format(client_name))
            except:
                print("file not found")
            try:
                os.remove("/etc/openvpn/crl.pem")
            except:
                print("file not found")
            shutil.copy("/etc/openvpn/easy-rsa/pki/crl.pem", "/etc/openvpn/crl.pem")
            os.chmod("/etc/openvpn/crl.pem", 644)

            try:
                os.remove("/home/{}.ovpn".format(client_name))
                print("removed")
            except Exception:
                print("check /home...")
            try:
                os.remove("/home{}/{}.ovpn".format(ENV["HOME"], client_name))
                print("removed")
            except Exception:
                print("check /home{}/...".format(ENV["HOME"]))
            try:
                os.remove("/root/{}.ovpn".format(client_name))
                print("removed")
            except Exception:
                print("check /root...")
            print("\nCertificate for client {} revoked".format(client_name))
            sys.exit(1)


        switch = {
            '1': main_option_for_new_user,
            '2': main_option_revoke_user,
            '3': sys.exit,
        }
        switch[option]()


else:
    os.system("clear")
    print("Welcome to the OpenVPN installer ")
    # OpenVPN setup and first user creation
    print("I need to ask you a few questions before starting the setup")
    print("You can leave the default options and just press enter if you are ok with them")
    print("\nI need to know the IPv4 address of the network interface you want OpenVPN listening to.")
    print(
        "If your server is running behind a NAT, (e.g. LowEndSpirit, Scaleway) leave the IP address as it is. (local/private IP)")
    print("Otherwise, it should be your public IPv4 address.")
    # Autodetect IP address and pre-fill for the user
    ip_address = os.popen("echo $(ip addr | grep 'inet' | grep -v inet6 | grep -vE '127\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}' | grep -oE '[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}' | head -1)",
        mode="r").read().strip()
    #input_address = input("IP address[default:{}]: ".format(ip_address)).strip()
    #ip_address = input_address if input_address != "" else ip_address
    print("\nWhat port do you want for OpenVPN?")
    #input_port = input("Port[default:1194]: ")
    pref_port = 1194 #if input_port == "" or len(input_port) < 2 else int(input_port)
    #  If $IP is a private IP address, the server must be behind NAT
    public_hostname = ""
    if len(re.findall('^(10\.|172\.1[6789]\.|172\.2[0-9]\.|172\.3[01]\.|192\.168)', ip_address)):
        print("\nThis server is behind NAT. What is the public IPv4 address or hostname?")
        public_hostname = "trashpanda.pl" #input("Public IP address / hostname: ")
    print("What protocol do you want for OpenVPN?\nUnless UDP is blocked, you should not use TCP (unnecessarily slower)")
    #PROTOCOL = "EMPTY"
    PROTOCOL = "UDP"
    while PROTOCOL != "UDP" and PROTOCOL != "TCP":
        PROTOCOL = input("Protocol [UDP/TCP]: ").strip()

    print("""
    What DNS do you want to use with the VPN?
    1) Current system resolvers (from /etc/resolv.conf)
    2) Cloudflare (Anycast: worldwide)
    3) Quad9 (Anycast: worldwide)
    4) FDN (France)
    5) DNS.WATCH (Germany)
    6) OpenDNS (Anycast: worldwide)
    7) Google (Anycast: worldwide)
    8) Yandex Basic (Russia)
    9) AdGuard DNS (Russia)
    """)
    DNS = "7"
    while (DNS != "1" and DNS != "2" and DNS != "3" and DNS != "4" and DNS != "5" and DNS != "6" and DNS != "7" and DNS != "8" and DNS != "9"):
        DNS = input("DNS [1-9]: ")
    CIPHER = "cipher AES-128-CBC"
    print("Size of Diffie-Hellman key use {} bits".format("3072"))
    DH_KEY_SIZE = "3072"
    print("size of RSA key - 3072 bits")
    RSA_KEY_SIZE = "3072"
    print("Finally, tell me a name for the client certificate and configuration")
    CLIENT = ""
    while CLIENT == "":
        print("Please, use one word only, no special characters")
        CLIENT = "DARWIN"#input("Client name: ")
    print("OK. that was all. Setup your OpenVPN server now")
    input("Press any key to continue...")
    print(ip_address, pref_port, PROTOCOL, CIPHER, DH_KEY_SIZE, RSA_KEY_SIZE, CLIENT, DNS, public_hostname)

    # Instalation OperationSystem

    if (OperationSystem == 'debian'):

        os.system("apt-get install ca-certificates gnupg -y")

        # We add the OpenVPN repo to get the latest version.
        # Debian 7
        def into_resolf_package_list(repository: str):
            with open("/etc/apt/source.list.d/openvpn.list", mode='w+') as file:
                file.write(repository)
            os.system("wget -O - https://swupdate.openvpn.net/repos/repo-public.gpg | apt-key add -")
            os.system("apt update")
            os.system("apt-get install openvpn iptables openssl wget ca-certificates curl -y")

        if version_id == 'VERSION_ID="7"':
            into_resolf_package_list("deb http://build.openvpn.net/debian/openvpn/stable wheezy main")
        if version_id == 'VERSION_ID="8"':
            into_resolf_package_list("deb http://build.openvpn.net/debian/openvpn/stable jessie main")
        if version_id == 'VERSION_ID="14.04"':
            into_resolf_package_list("deb http://build.openvpn.net/debian/openvpn/stable trusty main")
        os.system("apt-get install openvpn iptables openssl wget ca-certificates curl -y")
        if not os.path.exists("/etc/systemd/system/iptables.service"):
            try:
                os.mkdir("/etc/iptables")
            except:
                print("-")
            finally:
                "Create folder /etc/iptables"
            ip_table_save = os.popen("iptables-save", mode="r").read()
            with open("/etc/iptables/iptables.rules", "w+") as file:
                file.writelines(ip_table_save)

            with open("/etc/iptables/flush-iptables.sh", "w+") as file:
                file.write("""
                #!/bin/sh
                iptables -F
                iptables -X
                iptables -t nat -F
                iptables -t nat -X
                iptables -t mangle -F
                iptables -t mangle -X
                iptables -P INPUT ACCEPT
                iptables -P FORWARD ACCEPT
                iptables -P OUTPUT ACCEPT""")
            os.system("chmod +x /etc/iptables/flush-iptables.sh")
            with open("/etc/systemd/system/iptables.service", "w+") as file:
                file.write("""
                [Unit]
                Description=Packet Filtering Framework
                DefaultDependencies=no
                Before=network-pre.target
                Wants=network-pre.target
                [Service]
                Type=oneshot
                ExecStart=/sbin/iptables-restore /etc/iptables/iptables.rules
                ExecReload=/sbin/iptables-restore /etc/iptables/iptables.rules
                ExecStop=/etc/iptables/flush-iptables.sh
                RemainAfterExit=yes
                [Install]
                WantedBy=multi-user.target""")
            os.system("systemctl daemon-reload")
            os.system("systemctl enable iptables.service")

    if (OperationSystem == 'fedora'):
        os.system("yum install openvpn iptables openssl wget ca-certificates curl -y")
        # Install iptables service
        if os.path.exists("/etc/systemd/system/iptables.service"):
            os.mkdir("/etc/iptables")
            ip_table_save = os.popen("iptables-save", mode="").read()
            with open("/etc/iptables/iptables.rules", "w+") as file:
                file.writelines(ip_table_save)

        with open("/etc/iptables/flush-iptables.sh", "w+") as file:
            file.write("""#!/bin/sh
            iptables -F
            iptables -X
            iptables -t nat -F
            iptables -t nat -X
            iptables -t mangle -F
            iptables -t mangle -X
            iptables -P INPUT ACCEPT
            iptables -P FORWARD ACCEPT
            iptables -P OUTPUT ACCEPT""")
        os.system("systemctl daemon-reload")
        with open("/etc/systemd/system/iptables.service", "w+") as file:
            file.write("""
            [Unit]
            Description=Packet Filtering Framework
            DefaultDependencies=no
            Before=network-pre.target
            Wants=network-pre.target
            [Service]
            Type=oneshot
            ExecStart=/sbin/iptables-restore /etc/iptables/iptables.rules
            ExecReload=/sbin/iptables-restore /etc/iptables/iptables.rules
            ExecStop=/etc/iptables/flush-iptables.sh
            RemainAfterExit=yes
            [Install]
            WantedBy=multi-user.target""")
        os.system("systemctl daemon-reload")
        os.system("systemctl enable iptables.service")
        # Disable firewalld to allow iptables to start upon reboot
        os.system("systemctl disable firewalld")
        os.system("systemctl mask firewalld")

    # Find out f the machine uses nogroup or nobody for the permissionless group
    NOGROUP = "nogroup" if str(os.popen("less /etc/group", mode="r").read().strip()).find("nogroup:") > 0 else "nobody"


    # An old version of easy-rsa was available by default in some openvpn packages
    if os.path.exists("/etc/openvpn/easy-rsa/"):
        shutil.rmtree("/etc/openvpn/easy-rsa/")
    # Get easy-rsa
    os.system("wget -O ~/EasyRSA-3.0.4.tgz https://github.com/OpenVPN/easy-rsa/releases/download/v3.0.4/EasyRSA-3.0.4.tgz")
    os.system("tar xzf ~/EasyRSA-3.0.4.tgz -C ~/")
    os.system("mv ~/EasyRSA-3.0.4/ /etc/openvpn/")
    os.system("mv /etc/openvpn/EasyRSA-3.0.4/ /etc/openvpn/easy-rsa/")
    os.system("chown -R root:root /etc/openvpn/easy-rsa/")
    os.system("rm -f ~/EasyRSA-3.0.4.tgz")
    os.chdir("/etc/openvpn/easy-rsa/")

    # Generate a random, alphanumeric identifier of 16 characters for CN and one for server name
    x = lambda: "".join(
        [random.choice("qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890") for _ in range(15)])
    SERVER_CN = "cn_" + x()
    SERVER_NAME = "server_" + x()
    with open("/etc/openvpn/easy-rsa/vars", mode="w+") as file:
        file.writelines("""
        set_var EASYRSA_KEY_SIZE {}
        set_var EASYRSA_REQ_CN {}
        """.format(RSA_KEY_SIZE, SERVER_CN))
    # Create the PKI, set up the CA, the DH params and the server + client certificates
    os.system("/etc/openvpn/easy-rsa/easyrsa init-pki")
    os.system("/etc/openvpn/easy-rsa/easyrsa --batch build-ca nopass")
    os.system("openssl dhparam -out dh.pem {}".format(DH_KEY_SIZE))
    os.system("/etc/openvpn/easy-rsa/easyrsa build-server-full {} nopass".format(SERVER_NAME))
    os.system("/etc/openvpn/easy-rsa/easyrsa build-client-full {} nopass".format(CLIENT))
    EASYRSA_CRL_DAYS = 3650
    os.system("/etc/openvpn/easy-rsa/easyrsa gen-crl")
    # generate tls-auth key
    os.system("openvpn --genkey --secret /etc/openvpn/tls-auth.key")
    # Move all the generated files
    shutil.copy("/etc/openvpn/easy-rsa/pki/ca.crt", "/etc/openvpn")
    shutil.copy("/etc/openvpn/easy-rsa/pki/private/ca.key", "/etc/openvpn")
    shutil.copy("/etc/openvpn/easy-rsa/dh.pem", "/etc/openvpn")
    shutil.copy("/etc/openvpn/easy-rsa/pki/issued/{}.crt".format(SERVER_NAME), "/etc/openvpn")
    shutil.copy("/etc/openvpn/easy-rsa/pki/private/{}.key".format(SERVER_NAME), "/etc/openvpn")
    shutil.copy("/etc/openvpn/easy-rsa/pki/crl.pem", "/etc/openvpn")
    # Make cert revocation list readable for non-root
    os.chmod("/etc/openvpn/crl.pem", 644)
    server_text_config = "port {}\n".format(pref_port)
    server_text_config += "proto {}\n".format(PROTOCOL.lower())
    server_text_config += "dev tun\n"
    server_text_config += "user nobody\n"
    server_text_config += "group {}\n".format(NOGROUP)
    server_text_config += "persist-key\n"
    server_text_config += "persist-tun\n"
    server_text_config += "keepalive 10 120\n"
    server_text_config += "topology subnet\n"
    server_text_config += "server 10.8.0.0 255.255.255.0\n"
    server_text_config += "ifconfig-pool-persist ipp.txt\n"
    # DNS resolver
    switch2 = {
        """
        "1": []
        # Locate the proper resolv.conf
        # Needed for systems running systemd-resolved
        if grep -q "127.0.0.53" "/etc/resolv.conf"; then
            RESOLVCONF='/run/systemd/resolve/resolv.conf'
        else
            RESOLVCONF='/etc/resolv.conf'
        fi
        # Obtain the resolvers from resolv.conf and use them for OpenVPN
        grep -v '#' $RESOLVCONF | grep 'nameserver' | grep -E -o '[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}' | while read line; do
            echo "push \"dhcp-option DNS $line\"" >> /etc/openvpn/server.conf
        """
        "2": ["1.0.0.1", "1.1.1.1"],  # Cloudflare
        "3": ["9.9.9.9"],  # Quad9
        "4": ["80.67.169.40", "80.67.169.12"],  # FDN
        "5": ["84.200.69.80", "84.200.70.40"],  # DNS.WATCH
        "6": ["208.67.222.222", "208.67.220.220"],  # OpenDNS
        "7": ["8.8.8.8", "8.8.4.4"],  # Google
        "8": ["77.88.8.8", "77.88.8.1"],  # Yandex Basic
        "9": ["176.103.130.130", "176.103.130.131"]  # AdGuard DNS
    }
    server_text_config += "".join([ str('push "dhcp-option DNS {}"\n'.format(selectdns)) for selectdns in switch2[DNS]])
    server_text_config += 'push "redirect-gateway def1 bypass-dhcp"\n'
    server_text_config += 'crl-verify crl.pem\n'
    server_text_config += 'ca ca.crt\n'
    server_text_config += 'cert {}.crt\n'.format(SERVER_NAME)
    server_text_config += 'key {}.key\n'.format(SERVER_NAME)
    server_text_config += 'tls-auth tls-auth.key 0\n'
    server_text_config += 'dh dh.pem\n'
    server_text_config += 'auth SHA256\n'
    server_text_config += '{}\n'.format(CIPHER)
    server_text_config += 'tls-server\n'
    server_text_config += 'tls-version-min 1.2\n'
    server_text_config += 'tls-cipher TLS-DHE-RSA-WITH-AES-128-GCM-SHA256\n'
    server_text_config += 'status openvpn.log\n'
    server_text_config += 'verb 3\n'
    with open("/etc/openvpn/server.conf", mode="w+") as file:
        file.writelines(server_text_config)

    if not os.path.exists(SYSCTL):
        os.system("touch {}".format(SYSCTL))
    with open(SYSCTL, mode="r") as file:
        systemCtlFile = file.read()
    result = systemCtlFile.find("net.ipv4.ip_forward=")
    if result < 0:
        with open(SYSCTL, mode="a") as file:
            file.write("net.ipv4.ip_forward=1\n")
    else:
        pointer = systemCtlFile.find("net.ipv4.ip_forward=") + len("net.ipv4.ip_forward=")
        newSystemCtlFile = systemCtlFile[:(pointer)] + "1" + systemCtlFile[(pointer + 1):]
        with open(SYSCTL, mode="w+") as file:
            file.write(newSystemCtlFile)

    # Avoid an unneeded reboo
    with open("/proc/sys/net/ipv4/ip_forward", "w+") as file:
        file.write("1")
    print("IP FORWARD----------------------------")
        # Set NAT for the VPN subnet
    print("NIC:{}".format(NIC))
    os.system("iptables -t nat -A POSTROUTING -o {} -s 10.8.0.0/24 -j MASQUERADE".format(NIC))
    # Save persitent iptables rules
    ip_table_save = os.popen("iptables-save", mode="r").read()
    with open(IPTABLES, "w+") as file:
        file.write(ip_table_save)


    print("DEBIAN------------------------------------")
    # TODO: write searching by proces name.
    if "" != os.popen("pgrep firewalld", mode="r").read().strip():
        # We don't use --add-service=openvpn because that would only work with
        # the default port. Using both permanent and not permanent rules to
        #  avoid a firewalld reload.
        os.system("firewall-cmd --zone=public --add-port={}/{}".format(pref_port, PROTOCOL.lower()))
        os.system("firewall-cmd --permanent --zone=public --add-port={}/{}".format(pref_port, PROTOCOL.lower()))
        os.system("firewall-cmd --zone=trusted --add-source=10.8.0.0/24")
        os.system("firewall-cmd --permanent --zone=trusted --add-source=10.8.0.0/24")

    # TODO : prite searching by proces name
    if os.popen("iptables -L -n | grep -E 'REJECT|DROP'", mode="r").read().strip() != "":
        # If iptables has at least one REJECT rule, we asume this is needed.
        # Not the best approach but I can't think of other and this shouldn'
        # cause problems.
        os.system("iptables -I INPUT -p {0} --dport {1} -j ACCEPT".format(PROTOCOL, pref_port))
        os.system("iptables -I FORWARD -s 10.8.0.0/24 -j ACCEPT")
        os.system("iptables -I FORWARD -m state --state RELATED,ESTABLISHED -j ACCEPT")
        # Save persitent OpenVPN rules
        ip_table_save = os.popen("iptables-save", mode="r").read()
        with open(IPTABLES, "w+") as file:
            file.write(ip_table_save)
    print("DEBIAN1------------------------------------")
    # And finally, restart OpenVPN
    if OperationSystem == 'debian':
        # Little hack to check for systemd
        if "" != os.popen("pgrep systemd-journal", mode="r").read().strip():
            # Workaround to fix OpenVPN service on OpenVZ
            os.system("sed -i 's|LimitNPROC|#LimitNPROC|' /lib/systemd/system/openvpn\@.service")
            os.system("sed -i 's|/etc/openvpn/server|/etc/openvpn|' /lib/systemd/system/openvpn\@.service")
            os.system("sed -i 's|%i.conf|server.conf|' /lib/systemd/system/openvpn\@.service")
            os.system("systemctl daemon-reload")
            os.system("systemctl restart openvpn")
            os.system("systemctl enable openvpn")
        else:
            os.system("/etc/init.d/openvpn restart")

    else:
        if "" != os.popen("pgrep systemd-journal", mode="r").read().strip():
            if OperationSystem == 'arch' or OperationSystem == 'fedora':
                # Workaround to avoid rewriting the entire script for Arch & Fedora
                os.system("sed -i 's|/etc/openvpn/server|/etc/openvpn|' /usr/lib/systemd/system/openvpn-server@.service")
                os.system("sed -i 's|%i.conf|server.conf|' /usr/lib/systemd/system/openvpn-server@.service")
                os.system("systemctl daemon-reload")
                os.system("systemctl restart openvpn-server@openvpn.service")
                os.system("systemctl enable openvpn-server@openvpn.service")
            else:
                os.system("systemctl restart openvpn@server.service")
                os.system("systemctl enable openvpn@server.service")
        else:
            os.system("service openvpn restart")
            os.system("chkconfig openvpn on")

    # If the server is behind a NAT, use the correct IP address
    # if "$PUBLICIP" != "" then
    #	IP=$PUBLICIP

    # client-template.txt is created so we have a template to add further users later
    with open("/etc/openvpn/client-template.txt", mode="w+") as file:
        string = "client\n"
        string += "proto udp\n" if PROTOCOL == 'UDP' else "proto tcp-client\n"
        string += 'remote {} {}\n'.format(ip_address, pref_port)
        string += 'dev tun\n'
        string += 'resolv-retry infinite\n'
        string += 'nobind\n'
        string += 'persist-key\n'
        string += 'persist-tun\n'
        string += 'remote-cert-tls server\n'
        string += 'verify-x509-name {} name\n'.format(SERVER_NAME)
        string += 'auth SHA256\n'
        string += 'auth-nocache\n'
        string += '{}\n'.format(CIPHER)
        string += 'tls-client\n'
        string += 'tls-version-min 1.2\n'
        string += 'tls-cipher TLS-DHE-RSA-WITH-AES-128-GCM-SHA256\n'
        string += 'setenv opt block-outside-dns\n'
        string += 'verb 3\n'
        file.write(string)

    print("Complite!")
sys.exit(1)
