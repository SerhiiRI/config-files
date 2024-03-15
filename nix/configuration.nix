# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/nvme0n1";
  boot.loader.grub.useOSProber = true;
  boot.supportedFilesystems = [ "ntfs" ];

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Enable network manager applet
  programs.nm-applet.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Kyiv";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "uk_UA.UTF-8";
    LC_IDENTIFICATION = "uk_UA.UTF-8";
    LC_MEASUREMENT = "uk_UA.UTF-8";
    LC_MONETARY = "uk_UA.UTF-8";
    LC_NAME = "uk_UA.UTF-8";
    LC_NUMERIC = "uk_UA.UTF-8";
    LC_PAPER = "uk_UA.UTF-8";
    LC_TELEPHONE = "uk_UA.UTF-8";
    LC_TIME = "uk_UA.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the MATE Desktop Environment.
  services.xserver.displayManager.lightdm = {
    enable = true;
    extraConfig = ''
      logind-check-graphical=true
    '';
  };
  services.xserver.desktopManager.mate.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.serhii = {
    isNormalUser = true;
    description = "serhii";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    shell = pkgs.fish; # nushell;
    useDefaultShell = false;
    packages = with pkgs; [
      firefox
      pkgs.yarn
      pkgs.babashka
      pkgs.clojure
      pkgs.vivaldi
      pkgs.spotify
      pkgs.lxappearance
      pkgs.gimp-with-plugins
      pkgs.inkscape
      pkgs.jetbrains.datagrip
      pkgs.docker
      pkgs.evince
      pkgs.dmenu
      pkgs.wofi
      pkgs.wayland
      pkgs.hyprland
      pkgs.kitty
      pkgs.deadbeef
      pkgs.flameshot
      pkgs.slack
      pkgs.telegram-desktop
      pkgs.qbittorrent
      pkgs.cinnamon.nemo
      pkgs.mpv
      pkgs.nushell
      pkgs.fish
      pkgs.zulu
      
      pkgs.poetry
      pkgs.python310
      pkgs.python310Packages.pip
      pkgs.python310Packages.setuptools
      pkgs.libgccjit
      pkgs.libstdcxx5
      pkgs.stdenv.cc.cc.lib
      
      pkgs.pcre
      pkgs.stack
      pkgs.ghc
      haskellPackages.cabal-install
      pkgs.gnome.gnome-terminal
      pkgs.docker-compose
      pkgs.xorg.libX11
      pkgs.xorg.libXrandr
      # pkgs.haskellPackages.xmonad
      pkgs.haskellPackages.xmobar
      pkgs.hyprland
      pkgs.waybar
      pkgs.gnumake
      pkgs.ubuntu_font_family
      pkgs.jetbrains-mono
      pkgs.nitrogen
      pkgs.fd
    #  thunderbird
    ];
  };

  virtualisation.docker = {
    rootless = {
       enable = true;
       setSocketVariable = true;
    };
    enable = true;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  			     pkgs.wget
  			     pkgs.zip
			     pkgs.unzip
		             pkgs.git
                             pkgs.ntfs3g
  			     pkgs.emacs
  ];

  environment.sessionVariables = rec {
    SHELL    = "fish";
  };
  
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  #  programs.nushell.enable = true;
  programs.fish.enable = true;
  programs.hyprland = {
    # Install the packages from nixpkgs
    enable = true;
    # Whether to enable XWayland
    xwayland.enable = true;
  };
  
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  systemd.services.brightness = {
    enable = true;
    description = "Set Brightness writable to everybody";
    wantedBy = [ "multi-user.target" ];
    unitConfig = {
      Type = "oneshot";
      User = "root";
    };
    serviceConfig = {
      ExecStart = "/bin/sh -c 'chgrp -R -H users /sys/class/backlight/intel_backlight && chmod g+w /sys/class/backlight/intel_backlight/brightness'";
    };
  };
  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?

}

