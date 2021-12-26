-- Mail ------------------------------------------------------------------------
-- mail = hs.image.imageFromPath("~/.hammerspoon/bin/mail.png")
-- mail:setSize({w=16,h=16})
-- mailMenu = hs.menubar.new()
-- mailMenu:setIcon(mail)

-- function updateMailMenu()
--   local succeeded, result, desc = hs.osascript.applescript("tell application \"Mail\" to get the unread count of inbox")

--   if (succeeded and result ~= nil) then
--     local count = tostring(result)
--     mailMenu:setTitle(count)
--   end

--   local getUnreadList = [[
--     tell application "Mail"
--       set msgs to (get the messages 1 thru 10 of inbox)
--       set unreadMsgsIDs to {}
      
--       repeat with msg in msgs
--         if read status of msg is false then
--           set end of unreadMsgsIDs to (get id of msg)
--         end if
--       end repeat
--     end tell

--     return unreadMsgsIDs
--   ]]
--   local succeeded, result, desc = hs.osascript.applescript(getUnreadList)

--   if (succeeded and result ~= nil) then
--     local subjects = {}

--     for _,id in pairs(result) do
--       local readFn = function(id) 
--         return function (keys, item)
--           -- TODO Do something after run
--           local _, _, _ = hs.osascript.applescript("tell application \"Mail\" to set read status of (first message of inbox whose id is equal to " .. id .. ") to true")
--         end
--       end

--       local deleteFn = function(id)
--         return function (keys, item)
--           -- TODO Do something after run
--           local _, _, _ = hs.osascript.applescript("tell application \"Mail\" to delete (first message of inbox whose id is equal to " .. id .. ")")
--         end
--       end
      
--       local succeededSubject, subject, _ = hs.osascript.applescript("tell application \"Mail\" to get subject of (first message of inbox whose id is equal to " .. id .. ")")
--       local succeededSender, sender, _ = hs.osascript.applescript("tell application \"Mail\" to get sender of (first message of inbox whose id is equal to " .. id .. ")")
--       -- TODO Date sent

--       if (succeededSubject and subject ~= nil and succeededSender and sender ~= nil) then
--         local msgMenu = {}
--         table.insert(msgMenu, {title = "Delete", fn = deleteFn(id)})
--         table.insert(msgMenu, {title = "Mark as read", fn = readFn(id)})

--         table.insert(subjects, {title = sender .. " — " .. subject, menu = msgMenu})
--       end
--     end

--     mailMenu:setMenu(subjects)
--   end
-- end

-- updateMailMenu()
-- hs.timer.doEvery(1, updateMailMenu)
