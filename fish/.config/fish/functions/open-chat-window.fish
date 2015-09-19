function open-chat-window --description 'Opens a dmenu and then a gajim chat.'
	gajim-remote open_chat \
          (for account in (gajim-remote list_accounts)
             gajim-remote list_contacts $account
           end \
             | grep jid | sed 's/^.*: //' | sort | uniq | dmenu)
end
