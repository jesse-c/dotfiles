#!/bin/bash

cp ~/.config/MailMate/Contents/Resources/KeyBindings/User.plist /Applications/MailMate.app/Contents/Resources/KeyBindings
defaults write com.freron.MailMate MmNotificationDeleteButtonEnabled -bool YES
