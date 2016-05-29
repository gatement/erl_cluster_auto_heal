# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.define :node1 do |node1_config|
    node1_config.vm.box = "ubuntu/trusty64"
    node1_config.vm.boot_timeout = 600
    node1_config.vm.host_name = 'node1.local'
    node1_config.vm.network "private_network", ip: "192.168.2.11"
    node1_config.vm.provider "virtualbox" do |vb|
      vb.gui = false
      vb.memory = "512"
    end
  end

  config.vm.define :node2 do |node2_config|
    node2_config.vm.box = "ubuntu/trusty64"
    node2_config.vm.boot_timeout = 600
    node2_config.vm.host_name = 'node2.local'
    node2_config.vm.network "private_network", ip: "192.168.2.12"
    node2_config.vm.provider "virtualbox" do |vb|
      vb.gui = false
      vb.memory = "512"
    end
  end

  config.vm.define :node3 do |node3_config|
    node3_config.vm.box = "ubuntu/trusty64"
    node3_config.vm.boot_timeout = 600
    node3_config.vm.host_name = 'node3.local'
    node3_config.vm.network "private_network", ip: "192.168.2.13"
    node3_config.vm.provider "virtualbox" do |vb|
      vb.gui = false
      vb.memory = "512"
    end
  end

  config.vm.define :node4 do |node4_config|
    node4_config.vm.box = "ubuntu/trusty64"
    node4_config.vm.boot_timeout = 600
    node4_config.vm.host_name = 'node4.local'
    node4_config.vm.network "private_network", ip: "192.168.2.14"
    node4_config.vm.provider "virtualbox" do |vb|
      vb.gui = false
      vb.memory = "512"
    end
  end

  config.vm.define :node5 do |node5_config|
    node5_config.vm.box = "ubuntu/trusty64"
    node5_config.vm.boot_timeout = 600
    node5_config.vm.host_name = 'node5.local'
    node5_config.vm.network "private_network", ip: "192.168.2.15"
    node5_config.vm.provider "virtualbox" do |vb|
      vb.gui = false
      vb.memory = "512"
    end
  end

end
