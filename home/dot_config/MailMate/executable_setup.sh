#!/bin/bash

mkdir -p ~/Library/Application\ Support/MailMate/Resources/KeyBindings/

cp ~/.config/MailMate/KeyBindings/User.plist ~/Library/Application\ Support/MailMate/Resources/KeyBindings/

# https://manual.mailmate-app.com/hidden_preferences

defaults write com.freron.MailMate MmNotificationDeleteButtonEnabled -bool YES

defaults write com.freron.MailMate MmMessagesOutlineMoveStrategy -string "none"

defaults write com.freron.MailMate MmAutomaticallyExpandThreadsEnabled -bool YES
defaults write com.freron.MailMate MmAutomaticallyExpandOnlyWhenCounted -bool YES
