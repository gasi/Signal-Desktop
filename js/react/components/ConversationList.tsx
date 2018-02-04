// // TODO: How can we use CommonJS modules?
// import * as React from 'react';

// WORKAROUND: Use global `React` on `window`:
const React = window.React;
const {ConversationListItem} = window.Whisper.React;

interface Props {
  items: Array<Conversation>
}

interface Conversation {
  id: string
  type: string
  unreadCount: number
  verified: number
  tokens?: Array<string> | null
  active_at: number
  lastMessage: string
  timestamp: number
  profileKey: object
  profileName: string
  profileAvatar: ProfileAvatar
  name: string
  avatar: Avatar
  color?: null
  expireTimer?: null
  profileSharing: boolean
}

interface ProfileAvatar {
  data: object
  contentType: string
  size: number
}

interface Avatar {
  data: object
  contentType: string
  length: number
}

class ConversationList extends React.PureComponent<Props, {}> {
  render() {
    return (
      <div>
        {this.props.items.map((item, index) =>
          <ConversationListItem
            key={`ConversationListItem-${item.cid}`}
            avatarURL={item.avatarUrl}
            id={item.cid}
            isSelected={index % 10 === 0}
            name={item.get('name')}
            type={item.get('type')}
            lastMessage={item.get('lastMessage')}
            lastMessageTimestamp={item.get('timestamp')}
          />
        )}
      </div>
    );
  }
}

// WORKAROUND: Expose React component on global `window.Whisper.React`:
window.Whisper.React.ConversationList = ConversationList;
