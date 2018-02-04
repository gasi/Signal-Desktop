// // TODO: How can we use CommonJS modules?
// import * as React from 'react';

// WORKAROUND: Use global `React` on `window`:
const React = window.React;
const {ConversationListItem} = window.Whisper.React;

interface Props {
  items: Array<Conversation>
}

interface State {
  selectedItemId: string
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

class ConversationList extends React.PureComponent<Props, State> {
  constructor(props: Props) {
    super(props)

    this.state = {
      selectedItemId: null,
    }
  }

  handleItemClick = (event) => {
    this.setState({selectedItemId: event.id})
  }

  render() {
    return (
      <div>
        {this.props.items.map(item =>
          <ConversationListItem
            key={`ConversationListItem-${item.cid}`}
            avatarURL={item.avatarUrl}
            id={item.cid}
            isSelected={this.state.selectedItemId === item.cid}
            lastMessage={item.get('lastMessage')}
            lastMessageTimestamp={item.get('timestamp')}
            name={item.get('name')}
            onClick={this.handleItemClick}
            type={item.get('type')}
          />
        )}
      </div>
    );
  }
}

// WORKAROUND: Expose React component on global `window.Whisper.React`:
window.Whisper.React.ConversationList = ConversationList;
